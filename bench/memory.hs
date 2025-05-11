module Main (main) where

import Data.List
import Weigh
import General ( isProvableT, isProvableZ, FormM, FormP, Logic )
import CPL
import IPL2
import K
import K4
import GL
import S4
import PForm
import MForm


main :: IO ()
main = mainWith $ do
  -- mapM_
  --   ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
  --   allCasesP
  -- mapM_
  --   ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
  --   propCasesM
  mapM_
    ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
    boxesCasesM
  -- mapM_
  --   ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
  --   kCasesM
  -- mapM_
  --   ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
  --   k4CasesM
  -- mapM_
  --   ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
  --   glCasesM
  -- mapM_
  --   ( \ (label, logic, method, form, n) -> func label (method logic . form) n)
  --   s4CasesM


type Prover f = Logic f -> f -> Bool

allCasesP :: [(String, Logic FormP, Prover FormP, Int -> FormP, Int)]
allCasesP =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("CPL", classical)
                           , ("IPL", intui) 
                          ]
  , (formStr, formFor)  <- allFormulasP
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [100]
  , let result = method logic (formFor n)
  ]

propCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
propCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("K", k)
                           , ("K4", kfour)
                           , ("GL", gl)
                           , ("S4", sfour) ]
  , (formStr, formFor)  <- propFormulasM
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [100]
  , let result = method logic (formFor n)
  ]

boxesCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
boxesCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("K", k)
                           , ("K4", kfour)
                           , ("GL", gl)
                           , ("S4", sfour) ]
  , (formStr, formFor)  <- boxesFormulasM
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [1000]
  , let result = method logic (formFor n)
  ]

kCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
kCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("K", k) ]
  , (formStr, formFor)  <- kFormulasM
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [2]
  , let result = method logic (formFor n)
  ]

k4CasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
k4CasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("K4", kfour) ]
  , (formStr, formFor)  <- k4FormulasM
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [2]
  , let result = method logic (formFor n)
  ]

glCasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
glCasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("GL", gl) ]
  , (formStr, formFor)  <- glFormulasM
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [2]
  , let result = method logic (formFor n)
  ]

s4CasesM :: [(String, Logic FormM, Prover FormM, Int -> FormM, Int)]
s4CasesM =
  [ (intercalate "  " [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- [ ("S4", sfour) ]
  , (formStr, formFor)  <- s4FormulasM
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- [1000]
  , let result = method logic (formFor n)
  ]