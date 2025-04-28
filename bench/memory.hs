module Main (main) where

import Data.List
import Weigh
import General ( isProvableT, isProvableZ, FormM, FormP, Logic )
import CPL
import IPL
import ML hiding (func)
import K4
import PForm
import MForm


main :: IO ()
main = mainWith $ do
  mapM_
    ( \ (label, logic, method, form, k) -> func label (method logic . form) k)
    allCasesP
  mapM_
    ( \ (label, logic, method, form, k) -> func label (method logic . form) k)
    propCasesM
  mapM_
    ( \ (label, logic, method, form, k) -> func label (method logic . form) k)
    boxesCasesM
  mapM_
    ( \ (label, logic, method, form, k) -> func label (method logic . form) k)
    kCasesM

type Prover f = Logic f -> f -> Bool

allCasesP :: [(String, Logic FormP, Prover FormP, Int -> FormP, Int)]
allCasesP =
  [ (intercalate "  " [logicStr, formStr, methodStr, show k, show result], logic, method, formFor', k)
  | (logicStr, logic)   <- [ ("G3C", classical)
                           , ("G3I", intui) ]
  , (formStr, formFor')  <- allFormulasP
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , k <- [100]
  , let result = method logic (formFor' k)
  ]

propCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
propCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show k, show result], logic, method, formFor', k)
  | (logicStr, logic)   <- [ ("G3M", modal) ]
  , (formStr, formFor')  <- propFormulasM
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , k <- [100]
  , let result = method logic (formFor' k)
  ]

boxesCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
boxesCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show k, show result], logic, method, formFor', k)
  | (logicStr, logic)   <- [ ("G3M", modal) ]
  , (formStr, formFor')  <- boxesFormulasM
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , k <- [1000]
  , let result = method logic (formFor' k)
  ]

kCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
kCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show k, show result], logic, method, formFor', k)
  | (logicStr, logic)   <- [ ("G3M", modal) ]
  , (formStr, formFor')  <-kFormulasM
  , (methodStr, method) <- [ ("Tree", isProvableT)
                           , ("Zip ", isProvableZ) ]
  , k <- [20]
  , let result = method logic (formFor' k)
  ]

