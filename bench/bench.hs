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
import Example


-- disPhiPie
disPhiPieLG3Czip :: Int -> Bool
disPhiPieLG3Czip x = isProvableZ classical $ disPhiPieL x
disPhiPieLG3C :: Int -> Bool
disPhiPieLG3C x = isProvableT classical $ disPhiPieL x
disPhiPieRG3Czip :: Int -> Bool
disPhiPieRG3Czip x = isProvableZ classical $ disPhiPieR x
disPhiPieRG3C :: Int -> Bool
disPhiPieRG3C x = isProvableT classical $ disPhiPieR x

disPhiPieLG3Izip :: Int -> Bool
disPhiPieLG3Izip x = isProvableZ intui $ disPhiPieL x
disPhiPieLG3I :: Int -> Bool
disPhiPieLG3I x = isProvableT intui $ disPhiPieL x
disPhiPieRG3Izip :: Int -> Bool
disPhiPieRG3Izip x = isProvableZ intui $ disPhiPieR x
disPhiPieRG3I :: Int -> Bool
disPhiPieRG3I x = isProvableT intui $ disPhiPieR x

disPhiPieLG3Kzip :: Int -> Bool
disPhiPieLG3Kzip x = isProvableZ modal $ pTom . disPhiPieL $ x
disPhiPieLG3K :: Int -> Bool
disPhiPieLG3K x = isProvableT modal $ pTom . disPhiPieL $ x
disPhiPieRG3Kzip :: Int -> Bool
disPhiPieRG3Kzip x = isProvableZ modal $ pTom . disPhiPieR $ x
disPhiPieRG3K :: Int -> Bool
disPhiPieRG3K x = isProvableT modal $ pTom . disPhiPieR $ x

-- disPie
disPieLG3Czip :: Int -> Bool
disPieLG3Czip x = isProvableZ classical $ disPieL x
disPieLG3C :: Int -> Bool
disPieLG3C x = isProvableT classical $ disPieL x
disPieRG3Czip :: Int -> Bool
disPieRG3Czip x = isProvableZ classical $ disPieR x
disPieRG3C :: Int -> Bool
disPieRG3C x = isProvableT classical $ disPieR x

disPieLG3Izip :: Int -> Bool
disPieLG3Izip x = isProvableZ intui $ disPieL x
disPieLG3I :: Int -> Bool
disPieLG3I x = isProvableT intui $ disPieL x
disPieRG3Izip :: Int -> Bool
disPieRG3Izip x = isProvableZ intui $ disPieR x
disPieRG3I :: Int -> Bool
disPieRG3I x = isProvableT intui $ disPieR x

disPieLG3Kzip :: Int -> Bool
disPieLG3Kzip x = isProvableZ modal $ pTom . disPieL $ x
disPieLG3K :: Int -> Bool
disPieLG3K x = isProvableT modal $ pTom . disPieL $ x
disPieRG3Kzip :: Int -> Bool
disPieRG3Kzip x = isProvableZ modal $ pTom . disPieR $ x
disPieRG3K :: Int -> Bool
disPieRG3K x = isProvableT modal $ pTom . disPieR $ x

-- conPie
conPieRG3Czip :: Int -> Bool
conPieRG3Czip x = isProvableZ classical $ conPieR x
conPieRG3C :: Int -> Bool
conPieRG3C x = isProvableT classical $ conPieR x
conPieLG3Czip :: Int -> Bool
conPieLG3Czip x = isProvableZ classical $ conPieL x
conPieLG3C :: Int -> Bool
conPieLG3C x = isProvableT classical $ conPieL x

conPieRG3Izip :: Int -> Bool
conPieRG3Izip x = isProvableZ intui $ conPieR x
conPieRG3I :: Int -> Bool
conPieRG3I x = isProvableT intui $ conPieR x
conPieLG3Izip :: Int -> Bool
conPieLG3Izip x = isProvableZ intui $ conPieL x
conPieLG3I :: Int -> Bool
conPieLG3I x = isProvableT intui $ conPieL x

conPieRG3Kzip :: Int -> Bool
conPieRG3Kzip x = isProvableZ modal $ pTom . conPieR $ x
conPieRG3K :: Int -> Bool
conPieRG3K x = isProvableT modal $ pTom . conPieR $ x
conPieLG3Kzip :: Int -> Bool
conPieLG3Kzip x = isProvableZ modal $ pTom . conPieL $ x
conPieLG3K :: Int -> Bool
conPieLG3K x = isProvableT modal $ pTom . conPieL $ x

-- conBot
conBotLG3Czip :: Int -> Bool
conBotLG3Czip x = isProvableZ classical $ conBotL x
conBotLG3C :: Int -> Bool
conBotLG3C x = isProvableT classical $ conBotL x
conBotRG3Czip :: Int -> Bool
conBotRG3Czip x = isProvableZ classical $ conBotR x
conBotRG3C :: Int -> Bool
conBotRG3C x = isProvableT classical $ conBotR x

conBotLG3Izip :: Int -> Bool
conBotLG3Izip x = isProvableZ intui $ conBotL x
conBotLG3I :: Int -> Bool
conBotLG3I x = isProvableT intui $ conBotL x
conBotRG3Izip :: Int -> Bool
conBotRG3Izip x = isProvableZ intui $ conBotR x
conBotRG3I :: Int -> Bool
conBotRG3I x = isProvableT intui $ conBotR x

conBotLG3Kzip :: Int -> Bool
conBotLG3Kzip x = isProvableZ modal $ pTom . conBotL $ x
conBotLG3K :: Int -> Bool
conBotLG3K x = isProvableT modal $ pTom . conBotL $ x
conBotRG3Kzip :: Int -> Bool
conBotRG3Kzip x = isProvableZ modal $ pTom . conBotR $ x
conBotRG3K :: Int -> Bool
conBotRG3K x = isProvableT modal $ pTom . conBotR $ x

-- Modal Logic K
boxTopG3Kzip :: Int -> Bool
boxTopG3Kzip x = isProvableZ modal $ boxesTop x
boxTopG3K :: Int -> Bool
boxTopG3K x = isProvableT modal $ boxesTop x

formForKG3Kzip :: Int -> Bool
formForKG3Kzip x = isProvableZ modal $ formForK x
formForKG3K :: Int -> Bool
formForKG3K x = isProvableT modal $ formForK x
nFormForKG3Kzip :: Int -> Bool
nFormForKG3Kzip x = isProvableZ modal $ nFormForK x
nFormForKG3K :: Int -> Bool
nFormForKG3K x = isProvableT modal $ nFormForK x

benchMain :: IO ()
benchMain = do
  defaultMainWith myConfig (map mybench
    [ 
      ("ZipL"         , disPhiPieLG3Kzip  , map (10*) [1..10] )
    , ("TreeL"        , disPhiPieLG3K     , map (10*) [1..10] )
    , ("ZipR"         , disPhiPieRG3Kzip  , map (10*) [1..10] )
    , ("TreeR"        , disPhiPieRG3K     , map (10*) [1..10] ) 
    ])
  where
    mybench (name,f,range) = bgroup name $ map (run f) range
    run f k = bench (show k) $ whnf f k
    myConfig = defaultConfig { Criterion.Types.csvFile = Just theCSVname }

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
    putStrLn "moving away old results!"
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
    longify = longifyTo 14
    longifyTo n s = s ++ replicate (n - length s) ' '

