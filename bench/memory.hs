module Main (main) where

import Data.List
import Weigh
import General ( isProvableT, isProvableZ, FormM, FormP, Logic )
import CPL
import IPL
import K
-- import K4 -- TODO
import PForm
import MForm


main :: IO ()
main = mainWith $ do
  mapM_
    ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
    allCasesP
  mapM_
    ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
    propCasesM
  mapM_
    ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
    boxesCasesM
  mapM_
    ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
    kCasesM

type Prover f = Logic f -> f -> Bool

allCasesP :: [(String, Logic FormP, Prover FormP, Int -> FormP, Int)]
allCasesP =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor', n)
  | (logicStr, logic)   <- [ ("G3C", classical)
                           , ("G3I", intui) ]
  , (formStr, formFor')  <- allFormulasP
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , n <- [100]
  , let result = method logic (formFor' n)
  ]

propCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
propCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor', n)
  | (logicStr, logic)   <- [ ("G3M", k) ]
  , (formStr, formFor')  <- propFormulasM
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , n <- [100]
  , let result = method logic (formFor' n)
  ]

boxesCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
boxesCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor', n)
  | (logicStr, logic)   <- [ ("G3M", k) ]
  , (formStr, formFor')  <- boxesFormulasM
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , n <- [1000]
  , let result = method logic (formFor' n)
  ]

kCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
kCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor', n)
  | (logicStr, logic)   <- [ ("G3M", k) ]
  , (formStr, formFor')  <-kFormulasM
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , n <- [20]
  , let result = method logic (formFor' n)
  ]
