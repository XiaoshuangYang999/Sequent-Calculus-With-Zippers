module General where

import Basics
import Data.GraphViz
import Data.GraphViz.Types.Monadic hiding ((-->))
import Test.QuickCheck
import Data.List as List
import Data.Set
import qualified Data.Set as Set

type Sequent f = Set (Either f f)

-- Pretty printing a list of f's
ppList :: Show f => [f] -> String
ppList [] = ""
ppList [f] = show f
ppList (f:fs@(_:_)) = show f ++ " , " ++ ppList fs

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

type ProofWithH f = ([Sequent f], Proof f)

data ZipPath f = Top | Step (Sequent f) RuleName (ZipPath f) [Proof f] [Proof f]

data ZipProof f = ZP (Proof f) (ZipPath f)

type RuleT f = ProofWithH f -> Either f f -> [[ (RuleName,[Sequent f])]]

type RuleZ f = ZipProof f -> Either f f -> [[ (RuleName,[Sequent f])]]

data Logic f = Log
  { neg         :: f -> f
  , bot         :: f
  , isAtom      :: f -> Bool
  , isAxiom     :: Sequent f -> Bool  
  , safeRuleT   :: RuleT f
  , unsafeRuleT :: [RuleT f]  
  , safeRuleZ   :: RuleZ f
  , unsafeRuleZ :: [RuleZ f]
  }


-- * Tree-based prover
startForT :: f -> ProofWithH f
startForT f =  ([],Node (Set.singleton (Right f)) "" [])

isClosedPfT :: Eq f => ProofWithH f -> Bool
isClosedPfT (_,fs) = isClosedPf fs

isApplicableToT :: ProofWithH f -> Either f f -> RuleT f -> Bool
isApplicableToT fs f r = not . List.null $ r fs f

extendT  :: (Eq f, Show f, Ord f) => Logic f -> ProofWithH f -> [ProofWithH f ]
extendT l pt@(h, Node fs "" [])=
  case ( Left (bot l) `Set.member` fs,
        isAxiom l fs,
        Set.lookupMin $ Set.filter (\g -> isApplicableToT pt g (safeRuleT l)) fs,
        unsafeRuleT l
        )  of
  (True,_, _,_ )    -> [(h, Node fs "L⊥" [Proved])]
  -- fs is an axiom
  (_,True, _,_ )    -> [(h, Node fs "Ax" [Proved])]
  -- The safe rule can be applied
  (_   ,_,Just f,_ )   -> [
                    (h, Node fs therule (List.map snd ts) ) |
                    (therule,result) <- head $ safeRuleT l pt f,
                    ts <- pickOneOfEach [ extendT l (fs : h, Node nfs "" [])| nfs <- result]                    
                    ]
  -- The logic has no unsafe rule -> CPL
  (_   ,_,Nothing,[])  -> [(h, Node fs "" [])]
  -- The logic has an unsafe rule -> IPL, K
  (_   ,_,Nothing,r:_) -> case checkEmpty $ Set.filter (\g -> isApplicableToT pt g r) fs of
        -- No applicable formulas in fs
        Nothing -> [(h, Node fs "" [])]
        -- fs contains applicable formulas
        Just gs ->  if any isClosedPfT nps
                      then List.take 1 (List.filter isClosedPfT nps)
                      else [(h, Node fs "" [])]
          where
            nps = concat $ List.concatMap tryExtendT gs
            -- tryExtendT :: Either f f -> [[([Sequent f], Proof f)]]
            tryExtendT g = [  List.map (\pwh -> (h, Node fs therule [snd pwh]))
                                $ extendT l (fs : h, Node (head result) "" [])
                           | (therule,result) <- head $ (head.unsafeRuleT $ l) pt g ]
-- just for pattern matching
extendT l (h,Node fs r@(_:_) xs@(_:_))= [ (h,Node fs r $ List.map snd nfs)
                                        | nfs <- List.map (\f -> extendT l (fs:h,f)) xs]
extendT _ (h,Proved) = [(h,Proved)]
extendT _ (_,Node _ (_:_) [])= error"cannot have rules and no children"
extendT _ (_,Node _ [] (_:_)) = error"cannot have children and no rules"

proveT :: (Eq f, Show f,Ord f) => Logic f -> f -> [Proof f]
proveT l f = List.map snd $ extendT l (startForT f)

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

-- A function that tells whether a node has right siblings
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

isApplicableToZ :: ZipProof f -> Either f f -> RuleZ f -> Bool
isApplicableToZ fs f r = not . List.null $ r fs f

extendZ  :: (Ord f,Eq f) => Logic f -> ZipProof f -> [ZipProof f]
extendZ l zp@(ZP (Node fs "" []) p) =
  case ( Left (bot l) `Set.member` fs,
        isAxiom l fs,
        Set.lookupMin $ Set.filter (\g -> isApplicableToZ zp g (safeRuleZ l)) fs,
        unsafeRuleZ l)  of
  -- Switch the path if the current sequent is closed
  (True,_,_  ,_ )    -> extendZ l (switch (ZP (Node fs "L⊥" [Proved]) p))
  (_,True,_  ,_ )    -> extendZ l (switch (ZP (Node fs "Ax" [Proved]) p))
  -- Find a safe rule to use
  (_ ,_  ,Just f,_)     -> extendZ l (move_down $ ZP (Node fs therule ts) p) where
            (therule,result) = head . head $ safeRuleZ l zp f
            ts = [ Node nfs "" []| nfs <- result]
  -- Check if there is unsafe rule
        -- Whenever a dead end is found, stop the proving process
  (_ ,_  ,Nothing ,[])    -> [ZP (Node fs "" []) p]
        -- Has an unsafe rule
  (_,_   ,Nothing ,r:_)   -> case  checkEmpty $ Set.filter (\g -> isApplicableToZ zp g r) fs of
                -- Not applicable
                Nothing -> [ZP (Node fs "" []) p]
                -- Find a list of formulas to apply
                Just gs -> if any isClosedZP nps
                            then List.take 1 (List.filter isClosedZP nps)
                            else [ZP (Node fs "" []) p]
                      where
                        nps = concat $ List.concatMap tryExtendZ gs
                        tryExtendZ g = [ extendZ l (ZP (Node (head result) "" []) (Step fs therule p [] []) )
                          | (therule,result) <- head $ (head.unsafeRuleZ $ l) zp g ]
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

-- without built-in contraction; delete the original one
replaceRuleZ :: (Eq f,Ord f) => (Either f f -> [(RuleName,[Sequent f])]) -> RuleZ f
replaceRuleZ fun (ZP (Node fs "" []) _) g = 
    [[(fst . head $ fun g
    ,[ Set.delete g fs `Set.union` newfs | newfs <- snd . head $ fun g])] 
    | not (List.null (fun g))]
replaceRuleZ _ _ _ = []

replaceRuleT :: (Eq f,Ord f) => (Either f f -> [(RuleName,[Sequent f])]) -> RuleT f
replaceRuleT fun (_,Node fs "" []) g = 
    [[(fst . head $ fun g
    ,[ Set.delete g fs `Set.union` newfs | newfs <- snd . head $ fun g])] 
    | not (List.null (fun g))]
replaceRuleT _ _ _ = []


-- * The language
type Atom = Char

-- PL
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

-- Substitute f x t = f[t/x]
substituteP :: FormP -> FormP -> FormP -> FormP
substituteP BotP _ _ = BotP
substituteP f@(AtP _) x t = if f == x
                        then t
                        else f
substituteP (ConP f g) x t = ConP (substituteP f x t) (substituteP g x t)
substituteP (DisP f g) x t = DisP (substituteP f x t) (substituteP g x t)
substituteP (ImpP f g) x t = ImpP (substituteP f x t) (substituteP g x t)


-- ML
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

-- substitute f x t = f[x/t]
substituteM :: FormP -> FormP -> FormP -> FormP
substituteM BotP _ _ = BotP
substituteM f@(AtP _) x t = if f == x
                        then t
                        else f
substituteM (ConP f g) x t = ConP (substituteM f x t) (substituteM g x t)
substituteM (DisP f g) x t = DisP (substituteM f x t) (substituteM g x t)
substituteM (ImpP f g) x t = ImpP (substituteM f x t) (substituteM g x t)

-- Transform FormP to FormM
pTom :: FormP -> FormM
pTom BotP = BotM
pTom (AtP x) = AtM x
pTom (ConP x y) = ConM (pTom x) (pTom y)
pTom (DisP x y) = DisM (pTom x) (pTom y)
pTom (ImpP x y) = ImpM (pTom x) (pTom y)