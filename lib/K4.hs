module K4 (kfour) where

import qualified Data.Set as Set

import General
import K

kfour :: Logic FormM
kfour = Log { safeRules   = [leftBotM, isAxiomM, replaceRule safeML]
            , unsafeRules = [fourrule] }

-- | The 4 box rule.
fourrule :: Rule FormM
fourrule hs fs (Right (Box f)) = concatMap (globalLoopCheckMap hs) ss where
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
fourrule _ _ _ = []

-- Global loopcheck, seqs/= fs
globalLoopCheckMap :: History FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
globalLoopCheckMap hs seqs = [("☐4", [seqs]) | seqs `notElem` hs]