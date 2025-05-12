module Main where

import Control.Monad (when)
import Criterion.Main
import qualified Criterion.Types
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Csv
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Scientific
import qualified Data.Vector as V
import Numeric
import System.Directory

import General
import CPL
import IPL
import K
import GL
import K4
import S4
import PForm
import MForm

-- | Set of benchmarks for runtime.
allItems :: [Case]
allItems =
  -- selected formula set:
     makeCases [ ("IPL", intui) ] [("conPieL",conPieL), ("conPieR",conPieR)] [10,20..100] -- not provable
  ++ makeCases [ ("K", k) ] [("boxesTop",boxesTop)] [10,20..100] -- provable
  ++ makeCases [ ("K4", kfour) ] [("lobBoxes",lobBoxes)] [1..10] -- not provable
  ++ makeCases [ ("GL", gl) ] [("lobBoxes",lobBoxes)] [1..10] -- provable
  {-
  -- full formula set:
     makeCases [("CPL", classical), ("IPL", intui) ] allFormulasP [10] -- ,20..100
  ++ makeCases [("K", k)] (propFormulasM ++ boxesFormulasM ++ kFormulasM) [10] -- ,20..100
  ++ makeCases [("K4", kfour)] propFormulasM [10] -- ,20..100
  ++ makeCases [("GL", gl)] propFormulasM [10]
  ++ makeCases [("S4", sfour)] propFormulasM [10]
  -}

type Case = (String, Int -> Bool, [Int])

makeCases :: (Ord f, Show f) => [(String, Logic f)] -> [(String, Int -> f)] -> [Int] -> [Case]
makeCases logics forms sizes =
  [ (fS ++ "-" ++ lS ++ "-" ++ pS , prover logic . formula, sizes)
  | (fS, formula) <- forms
  , (pS, prover) <- [("GenZ", isProvableZ), ("GenT", isProvableT)]
  , (lS, logic) <- logics ]

benchMain :: IO ()
benchMain = defaultMainWith myConfig (map mybench allItems) where
  mybench (name,f,range) = bgroup name $ map (run f) range
  run f n = bench (show n) $ whnf f n
  myConfig = defaultConfig
    { Criterion.Types.csvFile = Just theCSVname
    , Criterion.Types.timeLimit = 10 }

main :: IO ()
main = prepareMain >> benchMain >> convertMain

-- * CSV to pgfplots

-- | The filename to which the benchmark results will be written in CSV.
theCSVname :: String
theCSVname = "bench/results.csv"

prepareMain :: IO ()
prepareMain = do
  oldResults <- doesFileExist theCSVname
  when oldResults $ do
    putStrLn "Note: moving away old results."
    renameFile theCSVname (theCSVname ++ ".OLD")
    oldDATfile <- doesFileExist (theCSVname ++ ".dat")
    when oldDATfile $ removeFile (theCSVname ++ ".dat")

-- | Convert the .csv file to a .dat file to be use with pgfplots.
convertMain :: IO ()
convertMain = do
  putStrLn "Reading results.csv and converting to .dat for pgfplots."
  c <- BL.readFile theCSVname
  case decode NoHeader c of
    Left err -> error $ "could not parse the csv file:" ++ show err
    Right csv -> do
      let results = map (parseLine . take 2) $ tail $ V.toList (csv :: V.Vector [String])
      let columns = nub.sort $ map (fst.fst) results
      let widthNeeded = maximum $ map length columns
      let longify = longifyTo (widthNeeded + 2)
      let firstLine = longifyTo 5 "n" ++ dropWhileEnd isSpace (concatMap longify columns)
      let resAt n col = longify $ fromMaybe "nan" $ Data.List.lookup (col,n) results
      let resultrow n = concatMap (resAt n) columns
      let firstcol = nub.sort $ map (snd.fst) results
      let resultrows = map (\n -> longifyTo 5 (show n) ++ dropWhileEnd isSpace (resultrow n)) firstcol
      writeFile (theCSVname ++ ".dat") (intercalate "\n" (firstLine:resultrows) ++ "\n")
  where
    parseLine [namestr,numberstr] = case splitOn "/" namestr of
      [name,nstr] -> ((name,n),valuestr) where
        n = read nstr :: Integer
        value = toRealFloat (read numberstr :: Scientific) :: Double
        valuestr = Numeric.showFFloat (Just 7) value ""
      _ -> error $ "could not parse this case: " ++ namestr
    parseLine l = error $ "could not parse this line:\n  " ++ show l
    longifyTo n s = s ++ replicate (n - length s) ' '
