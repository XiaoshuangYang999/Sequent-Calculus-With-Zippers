module S4 (sfour) where

import qualified Data.Set as Set

import General
import K (safeML)

sfour :: Logic FormM
sfour = Log { safeRules   = [leftBotM, isAxiomM, replaceRule safeML]
            , unsafeRules = [fourrule, trule] }

-- | The 4 box rule.
fourrule :: Rule FormM
fourrule hs fs (Right (Box f)) = concatMap (loopCheckMap hs) ss where
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
fourrule _ _ _ = []

-- | The T box rule.
trule :: Rule FormM
trule hs fs (Left (Box f)) = [("T", [Set.insert (Left f) fs]) | Set.insert (Left f) fs `notElem` hs]
trule _ _ _ = []

loopCheckMap :: History FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
loopCheckMap hs seqs = [("4", [seqs]) | seqs `notElem` hs]

isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False

fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left (Box g)) = Left g
fromBox g = g
