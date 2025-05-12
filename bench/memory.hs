module Main (main) where

import Data.List
import Weigh

import General (isProvableT, isProvableZ, Logic)
import CPL
import IPL2
import K
import K4
import GL
import S4
import PForm
import MForm

type Prover f = Logic f -> f -> Bool

type Case f = [(String, Logic f, Prover f, Int -> f, Int)]

run :: (Ord f, Show f) => [(String, Logic f)] -> [(String, Int -> f)] -> [Int] -> Weigh ()
run logics forms sizes =
  mapM_ (\ (label, logic, method, form, n) -> func label (method logic . form) n)
    $ makeCases logics forms sizes

main :: IO ()
main = mainWith $ do
  run [ ("CPL", classical), ("IPL", intui) ] allFormulasP [10]
  run [ ("K", k), ("K4", kfour), ("GL", gl), ("S4", sfour) ] propFormulasM [10]
  run [ ("K", k), ("K4", kfour), ("GL", gl), ("S4", sfour) ] boxesFormulasM [100]
  run [ ("K", k) ] kFormulasM [2]
  run [ ("K4", kfour) ] k4FormulasM [2]
  run [ ("GL", gl) ] glFormulasM [2]
  run [ ("S4", sfour) ] s4FormulasM [100]

makeCases :: (Ord f, Show f) => [(String, Logic f)] -> [(String, Int -> f)] -> [Int] -> Case f
makeCases logics forms sizes =
  [ (intercalate "|" [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- logics
  , (formStr, formFor)  <- forms
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- sizes
  , let result = method logic (formFor n)
  ]
