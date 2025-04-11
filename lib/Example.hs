{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Example where

import General
import Data.List as List

-- * Propositional tests

top,o,p,q,r :: FormP
top = negP BotP
[o,p,q,r] = List.map AtP ['o','p','q','r']

-- | Excluded middle
em :: FormP
em = DisP p (negP p)

-- | Double negation
dn :: FormP
dn = iffP (negP (negP p)) p

-- | Right Double negation
dnR :: FormP
dnR = ImpP p (negP (negP p))

-- | Pierce's Law
pierce :: FormP
pierce = ImpP (ImpP (ImpP p q) p) p

-- | List of tests
t1,t2,t3,t4 :: FormP
[t1,t2,t3,t4] = [ ImpP p p
                , negP (negP em)
                , ImpP (ImpP p (ImpP p q)) (ImpP p q)
                , ImpP (ImpP pierce q) q]

phi :: FormP
phi = ImpP (ConP p (ImpP p q)) (ImpP (ImpP p q) q)

-- * Modal logic tests

a1,b1,c1,d1,e1 :: FormM
[a1,b1,c1,d1,e1] = map AtM ['a','b','c','d','e']

kaxiom :: FormM
kaxiom = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (Box b1))

ktest :: FormM
ktest = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (ImpM (Box b1) (Box c1)))

-- * For benchmarks

-- ** Propositional Logic

disPhiPieR :: Int -> FormP
disPhiPieR k = foldr DisP phi (replicate (2*k) pierce )

disPhiPieL :: Int -> FormP
disPhiPieL k = foldl DisP phi (replicate (2*k) pierce )

disPieR :: Int -> FormP
disPieR k = foldr DisP pierce (replicate (2*k) pierce )

disPieL :: Int -> FormP
disPieL k = foldl DisP pierce (replicate (2*k) pierce )

conPieR :: Int -> FormP
conPieR k = foldr ConP pierce (replicate (2*k) pierce )

conPieL :: Int -> FormP
conPieL k = foldl ConP pierce (replicate (2*k) pierce )

conBotR :: Int -> FormP
conBotR k = foldr ConP BotP (replicate k BotP )

conBotL :: Int -> FormP
conBotL k = foldl ConP BotP (replicate k BotP )

disBotR :: Int -> FormP
disBotR k = foldr DisP BotP (replicate k BotP )

disBotL :: Int -> FormP
disBotL k = foldl DisP BotP (replicate k BotP )

allFormulasP :: [(String, Int -> FormP)]
allFormulasP =
  [ ("disPhiPieR", disPhiPieR)
  , ("disPhiPieL", disPhiPieL)
  , ("disPieR", disPieR)
  , ("disPieL", disPieL)
  , ("conPieR", conPieR)
  , ("conPieL", conPieL)
  , ("conBotR", conBotR)
  , ("conBotL", conBotL)
  ]

-- ** Modal Logic

boxesTop :: Int -> FormM
boxesTop 0 = topM
boxesTop n = Box (boxesTop (n-1))

boxesBot :: Int -> FormM
boxesBot 0 = BotM
boxesBot n = Box (boxesBot(n-1))

listOfAt :: Int -> [FormM]
listOfAt n = map AtM $ take n ['c'..]

formForK :: Int -> FormM
formForK n = ImpM (Box (List.foldr ImpM (AtM 'a') (listOfAt n)))
                $ foldr (ImpM . Box) (Box (AtM 'a')) (listOfAt n)

nFormForK :: Int -> FormM
nFormForK n = ImpM (Box (List.foldr ImpM (AtM 'a') (listOfAt n ++ [AtM 'b'])))
                $ foldr (ImpM . Box) (Box (AtM 'a')) (listOfAt n)

propFormulasM :: [(String, Int -> FormM)]
propFormulasM =
  [ ("disPhiPieR", pTom . disPhiPieR)
  , ("disPhiPieL", pTom . disPhiPieL)
  , ("disPieR", pTom . disPieR)
  , ("disPieL", pTom . disPieL)
  , ("conPieR", pTom . conPieR)
  , ("conPieL", pTom . conPieL)
  , ("conBotR", pTom . conBotR)
  , ("conBotL", pTom . conBotL)
  ]

boxesFormulasM :: [(String, Int -> FormM)]
boxesFormulasM =
  [ ("boxesTop", boxesTop)
  , ("boxesBot", boxesBot)
  ]

kFormulasM :: [(String, Int -> FormM)]
kFormulasM =
  [ ("formForK", formForK)
  , ("nFormForK", nFormForK)
  ]
