module Basics where

import Data.GraphViz
import Data.GraphViz.Types.Monadic hiding ((-->))
import GHC.IO.Handle
import System.IO
import System.IO.Temp
import System.IO.Unsafe
import qualified Data.ByteString as SB
import Data.Either
import Data.Set
import qualified Data.Set as Set


-- Displayable things, using graphviz.
class DispAble t where
  toGraph :: t -> DotM String ()
  disp :: t -> IO ()
  disp x = runGraphvizCanvas Dot (digraph' $ toGraph x) Xlib
  dot :: t -> IO ()
  dot x = graphvizWithHandle Dot (digraph' $ toGraph x) Canon $ \h -> do
    hSetEncoding h utf8
    SB.hGetContents h >>= SB.putStr
  svg :: t -> String
  svg x = unsafePerformIO $ withSystemTempDirectory "tapdleau" $ \tmpdir -> do
    _ <- runGraphvizCommand Dot (digraph' $ toGraph x) Svg (tmpdir ++ "/temp.svg")
    readFile (tmpdir ++ "/temp.svg")
  pdf :: t -> IO FilePath
  pdf x = runGraphvizCommand Dot (digraph' $ toGraph x) Pdf "temp.pdf"

-- Zipper for trees
class TreeLike z where
  zsingleton :: a -> z a
  move_left :: z a -> z a
  move_right :: z a -> z a
  move_up :: z a-> z a
  move_down :: z a -> z a
  zdelete :: z a -> z a

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

-- Pick one element of each list to form new lists.
pickOneOfEach :: [[a]] -> [[a]]
pickOneOfEach [] = [[]]
pickOneOfEach (l:ls) = [ x:xs | x <- l, xs <- pickOneOfEach ls ]

-- Set & Either
leftsSet :: Ord a => Set (Either a a) -> Set a
leftsSet xs = Set.map fromEither $ Set.filter isLeft xs

rightsSet :: Ord a => Set (Either a a) -> Set a
rightsSet xs = Set.map fromEither $ Set.filter isRight xs

leftOfSet :: Ord a => Set (Either a a) -> Set (Either a a)
leftOfSet = Set.filter isLeft 

rightOfSet :: Ord a => Set (Either a a) -> Set (Either a a)
rightOfSet = Set.filter isRight

-- To identify empty set
checkEmpty :: Set a -> Maybe (Set a)
checkEmpty xs
  | Set.null xs = Nothing
  | otherwise   = Just xs
