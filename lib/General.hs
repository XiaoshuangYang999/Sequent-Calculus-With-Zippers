{-# LANGUAGE InstanceSigs,TypeSynonymInstances #-}

module General where
import Basics
import Data.GraphViz
import Data.GraphViz.Types.Monadic hiding ((-->))
import Test.QuickCheck
import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

type Sequent f = Set (Either f f)

-- Pretty printing a list of f's
ppList :: Show f => [f] -> String
ppList = intercalate " , " . map show

-- Pretty printing a set of f's
ppForm :: Show f => Set f -> String
ppForm ms = ppList (Set.toList ms)

-- Pretty printing a sequent of f;s
ppSeq :: (Show f, Ord f) => Sequent f -> String
ppSeq xs = ppForm (leftsSet xs) ++ " => " ++ ppForm (rightsSet xs)

type RuleName = String
data Proof f = Proved | Node (Sequent f) RuleName [Proof f]
  deriving (Eq,Ord,Show)

hasLeqTwoChildren :: Eq f => Proof f -> Bool
hasLeqTwoChildren Proved = True
hasLeqTwoChildren (Node _ _ ts) = length ts <= 2 && all hasLeqTwoChildren ts

isClosedPf :: Eq f => Proof f -> Bool
isClosedPf Proved = True
isClosedPf (Node _ _ ts) = ts /= [] && all isClosedPf ts

instance (Show f,Ord f) => (DispAble (Proof f)) where
  toGraph = toGraph' "" where
    toGraph' pref Proved =
      node pref [shape PlainText, toLabel "☐"]
    toGraph' pref (Node fs rule' ts) = do
      node pref [shape PlainText, toLabel $ ppSeq fs]
      mapM_ (\(t,y') -> do
        toGraph' (pref ++ show y' ++ ":") t
        edge pref (pref ++ show y' ++ ":") [toLabel rule']
        ) (zip ts [(0::Integer)..])

-- * Histories, Rules, Logics

type History f = [Sequent f]

class HasHistory a where
  histOf :: a f -> History f

-- | A `Rule` takes the history/branch, current sequent and an active formula.
-- It returns ways to apply a rule, each resulting in possibly multiple branches.
type Rule f = History f -> Sequent f -> Either f f -> [(RuleName,[Sequent f])]

-- | A replace rule only takes an active formula.
replaceRule :: (Eq f,Ord f) => (Either f f -> [(RuleName,[Sequent f])]) -> Rule f
replaceRule fun _ fs g =
  [ ( fst . head $ fun g
    , [ Set.delete g fs `Set.union` newfs | newfs <- snd . head $ fun g ]
    )
  | not (null (fun g)) ]

isApplicable :: History f -> Sequent f -> Either f f -> Rule f -> Bool
isApplicable hs fs f r = not . null $ r hs fs f

-- | A Logic for a formula type `f`.
data Logic f = Log
  { neg         :: f -> f
  , bot         :: f
  , isAtom      :: f -> Bool
  , isAxiom     :: Sequent f -> Bool
  , safeRule    :: Rule f
  , unsafeRules :: [Rule f]
  }

-- * Tree Proofs

newtype ProofWithH f = HP (History f, Proof f)

instance HasHistory ProofWithH where
  histOf (HP (hs, _)) = hs

hpSnd :: ProofWithH f -> Proof f
hpSnd (HP (_, pf)) = pf

-- * Zip Proofs

data ZipPath f = Top | Step (Sequent f) RuleName (ZipPath f) [Proof f] [Proof f]

data ZipProof f = ZP (Proof f) (ZipPath f)

instance HasHistory ZipPath where
  histOf :: ZipPath f -> History f
  histOf Top = []
  histOf (Step xs _ p _ _) = xs : histOf p

instance HasHistory ZipProof where
  histOf :: ZipProof f -> History f
  histOf (ZP _ zpath) = histOf zpath


-- * Tree-based prover

startForT :: f -> ProofWithH f
startForT f =  HP ([],Node (Set.singleton (Right f)) "" [])

isClosedPfT :: Eq f => ProofWithH f -> Bool
isClosedPfT (HP (_,fs)) = isClosedPf fs

extendT  :: (Eq f, Show f, Ord f) => Logic f -> ProofWithH f -> [ProofWithH f]
extendT l pt@(HP (h, Node fs "" [])) =
  case ( Left (bot l) `Set.member` fs
       , isAxiom l fs
       , Set.lookupMin $ Set.filter (\g -> isApplicable h fs g (safeRule l)) fs
       , unsafeRules l ) of
  (True, _   , _     ,_ ) -> [HP (h, Node fs "L⊥" [Proved])] -- Left side contains ⊥.
  (_   , True, _     ,_ ) -> [HP (h, Node fs "Ax" [Proved])] -- We have an axiom.
  (_   , _   , Just f,_ ) -> -- The safe rule can be applied
    [ HP (h, Node fs therule (map hpSnd ts) )
    | (therule, result) <- safeRule l h fs f
    , ts <- pickOneOfEach [ extendT l (HP (fs : h, Node nfs "" []))
                          | nfs <- result ] ]
  -- The logic has no unsafe rule -> CPL: -- FIXME rephrase
  (_   ,_,Nothing,[])  -> [HP (h, Node fs "" [])]
  -- The logic has an unsafe rule -> IPL, K:
  (_   ,_,Nothing,r:_) -> case checkEmpty $ Set.filter (\g -> isApplicable h fs g r) fs of
        Nothing -> [HP (h, Node fs "" [])] -- No applicable formulas in fs
        Just gs ->  if any isClosedPfT nps -- fs contains applicable formulas
                      then List.take 1 (List.filter isClosedPfT nps)
                      else [HP (h, Node fs "" [])]
          where
            nps = concat $ List.concatMap tryExtendT gs
            tryExtendT g = [ List.map (\pwh -> HP (h, Node fs therule [hpSnd pwh]))
                                $ extendT l (HP (fs : h, Node (head result) "" []))
                           | (therule,result) <- r (histOf pt) fs g ]
-- just for pattern matching
extendT l (HP (h,Node fs r@(_:_) xs@(_:_))) = -- FIXME can we get rid of this?
  [ HP (h,Node fs r $ List.map hpSnd nfs)
  | nfs <- List.map (\f -> extendT l (HP (fs:h,f))) xs]
extendT _ (HP (h,Proved)) = [HP (h,Proved)]
extendT _ (HP (_,Node _ (_:_) [])) = error"cannot have rules and no children"
extendT _ (HP (_,Node _ [] (_:_))) = error"cannot have children and no rules"

proveT :: (Eq f, Show f,Ord f) => Logic f -> f -> [Proof f]
proveT l f = List.map hpSnd $ extendT l (startForT f)

isProvableT :: (Eq f, Show f, Ord f) => Logic f -> f -> Bool
isProvableT l f = any isClosedPf (proveT l f)

proveprintT :: (Eq f, Show f,Ord f) => Logic f -> f -> Proof f
proveprintT l f = if isProvableT l f
                then head $ List.filter isClosedPf (proveT l f)
                else head (proveT l f)

provePdfT :: (Ord f,Show f, Eq f) => Logic f -> f -> IO FilePath
provePdfT l f= pdf $ proveprintT l f


-- * Zipper-based prover
instance TreeLike ZipProof where
  zsingleton x                             = ZP (Node (Set.singleton (Right x)) "" []) Top
  move_left (ZP c (Step s r p (x:xs) ys))  = ZP x (Step s r p xs (c:ys))
  move_left _                              = error "cannot go left"
  move_right (ZP c (Step s r p xs (y:ys))) = ZP y (Step s r p (c:xs) ys)
  move_right _                             = error "cannot go right"
  move_up (ZP c (Step s r p xs ys))        = ZP (Node s r ((c:xs) ++ ys)) p
  move_up _                                = error "cannot go up"
  move_down (ZP (Node s r (x:xs)) p)       = ZP x (Step s r p [] xs)
  move_down _                              = error "cannot go down"
  zdelete (ZP _ (Step s _ Top _ _))        = ZP (Node s "" []) Top
  zdelete (ZP _ (Step s _ p _ _))          = ZP (Node s "" []) p
  zdelete _                                = error "cannot delete top"

fromZip :: ZipProof f -> Proof f
fromZip (ZP x Top) = x
fromZip zp = fromZip (move_up zp)

-- | Does the node have a right sibling?
hasRsibi :: ZipPath f -> Bool
hasRsibi (Step _ _ _ _ (_:_))= True
hasRsibi _ = False

-- Switch path, left-biased
switch :: ZipProof f -> ZipProof f
switch (ZP pf Top) = ZP pf Top
switch (ZP pf p) = if hasRsibi p
                      then move_right (ZP pf p)
                      else switch.move_up $ ZP pf p

startForZ :: f -> ZipProof f
startForZ f = ZP (Node (Set.singleton (Right f)) "" []) Top

isClosedZP :: Eq f => ZipProof f -> Bool
isClosedZP (ZP fs Top) = isClosedPf fs
isClosedZP (ZP fs p) = isClosedPf fs &&  (isClosedZP . switch $ ZP fs p)


extendZ  :: (Ord f,Eq f) => Logic f -> ZipProof f -> [ZipProof f]
extendZ l zp@(ZP (Node fs "" []) p) =
  case ( Left (bot l) `Set.member` fs
       , isAxiom l fs
       , Set.lookupMin $ Set.filter (\g -> isApplicable (histOf zp) fs g (safeRule l)) fs
       , unsafeRules l) of
  -- Switch the path if the current sequent is closed
  (True,_,_  ,_ )    -> extendZ l (switch (ZP (Node fs "L⊥" [Proved]) p))
  (_,True,_  ,_ )    -> extendZ l (switch (ZP (Node fs "Ax" [Proved]) p))
  -- Find a safe rule to use
  (_ ,_  ,Just f,_)     -> extendZ l (move_down $ ZP (Node fs therule ts) p) where
            (therule,result) = head $ safeRule l (histOf zp) fs f
            ts = [ Node nfs "" []| nfs <- result]
  -- Check if there is unsafe rule
        -- Whenever a dead end is found, stop the proving process
  (_ ,_  ,Nothing ,[])    -> [ZP (Node fs "" []) p]
        -- Has an unsafe rule
  (_,_   ,Nothing ,r:_)   -> case  checkEmpty $ Set.filter (\g -> isApplicable (histOf zp) fs g r) fs of
                -- Not applicable
                Nothing -> [ZP (Node fs "" []) p]
                -- Find a list of formulas to apply
                Just gs -> if any isClosedZP nps
                            then List.take 1 (List.filter isClosedZP nps)
                            else [ZP (Node fs "" []) p]
                      where
                        nps = concat $ List.concatMap tryExtendZ gs
                        tryExtendZ g = [ extendZ l (ZP (Node (head result) "" []) (Step fs therule p [] []) )
                          | (therule,result) <- r (histOf zp) fs g ]
extendZ _ (ZP Proved p) = [ZP Proved p]
extendZ _ (ZP (Node _ (_:_) []) _ )= error"cannot have rules and no children"
extendZ _ (ZP (Node fs r@(_:_) xs@(_:_)) p )= [ZP (Node fs r xs) p]
extendZ _ (ZP (Node _ [] (_:_)) _) = error"cannot have children and no rules"

proveZ :: (Eq f, Ord f) => Logic f -> f -> [Proof f]
proveZ l f = List.map fromZip $ extendZ l (startForZ f)

isProvableZ :: (Eq f, Ord f) => Logic f -> f -> Bool
isProvableZ l f = any isClosedZP $ extendZ l (startForZ f)

proveprintZ :: (Eq f, Ord f) => Logic f -> f -> Proof f
proveprintZ l f = if isProvableZ l f
                then head $  List.filter isClosedPf (proveZ l f)
                else head (proveZ l f)

provePdfZ :: (Show f, Eq f,Ord f) => Logic f -> f -> IO FilePath
provePdfZ l f = pdf $ proveprintZ l f

-- * The Propositional language

type Atom = Char

-- | Propositional Formulas
data FormP = BotP | AtP Atom | ConP FormP FormP | DisP FormP FormP | ImpP FormP FormP
  deriving (Eq,Ord)

isatomP :: FormP -> Bool
isatomP (AtP _) = True
isatomP _ = False

isAxiomP :: Sequent FormP -> Bool
isAxiomP fs = any (\f -> isatomP f && Right f `Set.member` fs) (leftsSet fs)

negP :: FormP -> FormP
negP f = ImpP f BotP

topP :: FormP
topP = negP BotP

iffP :: FormP -> FormP -> FormP
iffP f g = ConP (ImpP f g) (ImpP g f)

instance (Show FormP) where
  show BotP       = "⊥"
  show (AtP a)    = [a]
  show (ConP f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
  show (DisP f g) = "(" ++ show f ++ " v " ++ show g ++ ")"
  show (ImpP f g) = "(" ++ show f ++ " → " ++ show g ++ ")"

instance Arbitrary FormP where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotP, AtP <$> choose ('p','t') ]
    genForm 1 = AtP <$> choose ('p','t')
    genForm n = oneof
      [ pure BotP
      , AtP <$> choose ('p','t')
      , ImpP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      ]

-- Conjunction on lists
conP :: [FormP] -> FormP
conP = List.foldr ConP topP

disP :: [FormP] -> FormP
disP = List.foldr DisP BotP

-- | Substitution in propositional formulas:
-- @substituteP f x t@ means we replace @x@ by @t@ in @f@.
substituteP :: FormP -> Atom -> FormP -> FormP
substituteP BotP _ _ = BotP
substituteP f@(AtP y) x t = if y == x then t else f
substituteP (ConP f g) x t = ConP (substituteP f x t) (substituteP g x t)
substituteP (DisP f g) x t = DisP (substituteP f x t) (substituteP g x t)
substituteP (ImpP f g) x t = ImpP (substituteP f x t) (substituteP g x t)

-- * The Modal Language

data FormM = BotM | AtM Atom | ConM FormM FormM | DisM FormM FormM | ImpM FormM FormM | Box FormM
  deriving (Eq,Ord)

isatomM :: FormM -> Bool
isatomM (AtM _) = True
isatomM _ = False

isAxiomM :: Sequent FormM -> Bool
isAxiomM fs = any (\f -> isatomM f && Right f `Set.member` fs) (leftsSet fs)

negM :: FormM -> FormM
negM f = ImpM f BotM

topM :: FormM
topM = negM BotM

iffM :: FormM -> FormM -> FormM
iffM f g = ConM (ImpM f g) (ImpM g f)

diaM :: FormM -> FormM
diaM f = negM $ Box $ negM f

instance (Show FormM) where
  show BotM       = "⊥"
  show (AtM a)    = [a]
  show (ConM f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
  show (DisM f g) = "(" ++ show f ++ " v " ++ show g ++ ")"
  show (ImpM f g) = "(" ++ show f ++ " → " ++ show g ++ ")"
  show (Box f)   = "(" ++ " ☐ " ++ show f ++ ")"

instance Arbitrary FormM where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotM, AtM <$> choose ('p','t') ]
    genForm 1 = AtM <$> choose ('p','t')
    genForm n = oneof
      [ pure BotM
      , AtM <$> choose ('p','t')
      , ImpM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , Box <$> genForm (n `div` factor)
      ]

-- conjunction on lists
conM :: [FormM] -> FormM
conM = List.foldr ConM topM

disM :: [FormM] -> FormM
disM = List.foldr DisM BotM

-- | Substitution in modal formulas:
-- @substituteM f x t@ means we replace @x@ by @t@ in @f@.
substituteM :: FormP -> Atom -> FormP -> FormP
substituteM BotP _ _ = BotP
substituteM f@(AtP y) x t = if y == x then t else f
substituteM (ConP f g) x t = ConP (substituteM f x t) (substituteM g x t)
substituteM (DisP f g) x t = DisP (substituteM f x t) (substituteM g x t)
substituteM (ImpP f g) x t = ImpP (substituteM f x t) (substituteM g x t)

-- * Embedding Propositional language into Modal language

pTom :: FormP -> FormM
pTom BotP = BotM
pTom (AtP x) = AtM x
pTom (ConP x y) = ConM (pTom x) (pTom y)
pTom (DisP x y) = DisM (pTom x) (pTom y)
pTom (ImpP x y) = ImpM (pTom x) (pTom y)
