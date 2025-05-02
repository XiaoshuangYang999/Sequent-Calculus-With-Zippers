module K (k, safeML) where

import qualified Data.Set as Set

import Basics
import General

k :: Logic FormM
k = Log { safeRules   = [leftBotM, isAxiomM, replaceRule safeML]
        , unsafeRules = [krule] }

-- | Propositional rules for Modal Logic.
safeML :: Either FormM FormM -> [(RuleName,[Sequent FormM])]
safeML (Left (ConM f g))  = [("L∧", [Set.fromList [Left f, Left g]])]
safeML (Left (DisM f g))  = [("Lv", map Set.singleton [Left f, Left g])]
safeML (Left (ImpM f g))  = [("L→", map Set.singleton [Left g, Right f])]
safeML (Right (ConM f g)) = [("R∧", map Set.singleton [Right f, Right g])]
safeML (Right (DisM f g)) = [("Rv", [Set.fromList [Right g, Right f]])]
safeML (Right (ImpM f g)) = [("R→", [Set.fromList [Right g, Left f]])]
safeML _                  = []

-- | The K rule.
krule :: Rule FormM
krule _ fs (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet . removeBoxLeft $ fs where
  removeBoxLeft :: Sequent FormM -> Sequent FormM
  removeBoxLeft  = setComprehension isLeftBox fromBox
  isLeftBox :: Either FormM FormM -> Bool
  isLeftBox (Left (Box _)) = True
  isLeftBox _              = False
  fromBox :: Either FormM FormM -> Either FormM FormM
  fromBox (Left (Box g)) = Left g
  fromBox g = g
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("K", [Set.insert (Right g) seqs])
krule _ _ _ = []
