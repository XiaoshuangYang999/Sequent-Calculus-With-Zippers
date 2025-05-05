module S4_2 where

import qualified Data.Set as Set

import General
import K (safeML)

sfour2 :: Logic FormM
sfour2 = Log { safeRules   = [leftBotM, isAxiomM, replaceRule safeML]
            , unsafeRules = [fourrule, trule] }

-- | The 4 box rule.
fourrule :: Rule FormM
fourrule _ fs (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet $ Set.filter isLeftBox fs where
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("K", [Set.insert (Right g) seqs])
fourrule _ _ _ = []

-- | The T box rule.
trule :: Rule FormM
trule hs fs (Left (Box f)) = [("T", [Set.insert (Left f) fs]) | Set.insert (Left f) fs `notElem` hs]
trule _ _ _ = []

isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False

fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left (Box g)) = Left g
fromBox g = g
