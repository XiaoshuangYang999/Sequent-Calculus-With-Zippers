{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MForm where

import General
import PForm
import Data.List as List
-- Added temporarily for ghci test purpose
-- import K
-- import K4
-- import S4 
-- import GL

a1,b1,c1,d1,e1 :: FormM
[a1,b1,c1,d1,e1] = map AtM ['a','b','c','d','e']

-- Holds in all modal logics
ktest :: FormM
ktest = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (ImpM (Box b1) (Box c1)))

-- Holds in all modal logics
modalMP :: FormM
modalMP = ImpM (ConM (Box a1) (Box (ImpM a1 b1))) (Box b1)


-- * Axioms

-- Holds in all modal logics
kaxiom :: FormM
kaxiom = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (Box b1))

-- Holds in K4,S4
fouraxiom :: FormM
fouraxiom = ImpM (Box a1) (Box (Box a1))

-- Holds in T,S4
taxiom :: FormM
taxiom = ImpM (Box (Box a1)) (Box a1)

-- Holds in GL
lobaxiom :: FormM
lobaxiom = ImpM (Box (ImpM (Box a1) a1)) (Box a1)

-- Holds in D, T, S4, S5
consistency :: FormM
consistency = negM . Box $ BotM

-- Holds in T, S4
density :: FormM
density = ImpM (Box (Box a1)) (Box a1)

-- * For benchmarks
boxes :: Int -> FormM -> FormM
boxes 0 f = f
boxes n f = Box (boxes (n-1) f)

boxesTop :: Int -> FormM
boxesTop n = boxes n topM

boxesBot :: Int -> FormM
boxesBot n = boxes n BotM

-- Holds in K4, S4
boxToMoreBox :: Int -> FormM
boxToMoreBox n = ImpM (boxes n a1) (boxes (n + 1) a1)

-- Holds in S4
boxToFewerBox :: Int -> FormM
boxToFewerBox n = ImpM (boxes (n + 1) a1) (boxes n a1)

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



