module K where

import Basics
import General
import qualified Data.Set as Set

k :: Logic FormM
k = Log { safeRules    = [leftBotM, isAxiomM,replaceRule safeML]
        , unsafeRules = [krule]
        }

-- | Propositional rules for Modal Logic.
safeML :: Either FormM FormM -> [(RuleName,[Sequent FormM])]
safeML (Left (ConM f g))  = [("L∧", [Set.insert (Left g)     $ Set.singleton (Left f)]  )]
safeML (Left (DisM f g))  = [("Lv", [Set.singleton (Left f)  , Set.singleton (Left g)]  )]
safeML (Left (ImpM f g))  = [("L→", [Set.singleton (Left g)  , Set.singleton (Right f)] )]
safeML (Right (ConM f g)) = [("R∧", [Set.singleton (Right f) , Set.singleton (Right g)] )]
safeML (Right (DisM f g)) = [("Rv", [Set.insert (Right g)    $ Set.singleton (Right f)] )]
safeML (Right (ImpM f g)) = [("R→", [Set.insert (Right g)    $ Set.singleton (Left f)]  )]
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
