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
import ML
import K4
import PForm
import MForm


-- | 8 propositional formula generators
propFormulas :: [(String, Int -> FormP)]
propFormulas= [ ("disPhiPie-R", disPhiPieL)
              , ("disPhiPie-R", disPhiPieR)
              , ("disPie-L", disPieL)
              , ("disPie-R", disPieR)
              , ("conPie-L", conPieL)
              , ("conPie-R", conPieR)
              , ("conBot-L", conBotL)
              , ("conBot-R", conBotR)
              ]

-- | 3 modal formula generators
modalFormulas :: [(String, Int -> FormM)]
modalFormulas = [ ("boxesTop", boxesTop)
                , ("formForK", formForK)
                , ("nFormForK", nFormForK)
                ]

propItems :: [(String, Int -> Bool)]
propItems =
  [ (fS ++ "-" ++ lS ++ "-" ++ pS , prover logic . formula)
  | (fS, formula) <- propFormulas
  , (pS, prover) <- [("Zip", isProvableZ), ("Tree", isProvableT)]
  , (lS, logic) <- [("G3C", classical), ("G3I", intui) ] ]

modalItems :: [(String, Int -> Bool)]
modalItems =
  [ (fS ++ "-" ++ lS ++ "-" ++ pS, prover logic . formula)
  | (fS, formula) <- modalFormulas ++ map (fmap (pTom .)) propFormulas
  , (pS, prover) <- [("Zip", isProvableZ), ("Tree", isProvableT)]
  , (lS, logic) <- [("G3K", modal)] ]

benchMain :: IO ()
benchMain = defaultMainWith myConfig (map mybench (propItems ++ modalItems)) where
  range = map (10*) [1..10]
  mybench (name,f) = bgroup name $ map (run f) range
  run f k = bench (show k) $ whnf f k
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
