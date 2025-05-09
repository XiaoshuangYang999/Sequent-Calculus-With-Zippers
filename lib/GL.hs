module GL (gl) where

import qualified Data.Set as Set
import K
import General

gl :: Logic FormM
gl = Log { safeRules   = [leftBotM, isAxiomM, isCycle, replaceRule safeML]
         , unsafeRules = [fourrule] }

isCycle :: Rule FormM
isCycle h fs _ = [("cycle", []) | fs `elem` h]

-- | The 4 box rule.
fourrule :: Rule FormM
fourrule _ fs (Right (Box f)) = concatMap func ss where
  func :: Sequent FormM -> [(RuleName,[Sequent FormM])]
  func seqs = [("☐4", [seqs])]
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
fourrule _ _ _ = []
