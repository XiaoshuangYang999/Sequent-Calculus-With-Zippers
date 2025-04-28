module K4 where

import Basics
import General
import qualified Data.Set as Set

kfour :: Logic FormM
kfour = Log
  { neg         = negM
  , bot         = BotM
  , isAtom      = isatomM
  , isAxiom     = isAxiomM
  , safeRule    = replaceRule safeML
  , unsafeRules = [kbox,fourbox]
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

-- | The K box rule.
kbox :: Rule FormM
kbox _ fs (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet . removeBoxLeft $ fs
  where
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("K", [Set.insert (Right g) seqs])
kbox _ _ _ = []

-- | The 4 box rule.
fourbox :: Rule FormM
fourbox _ fs (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet $ Set.filter isLeftBox fs
  where
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("4", [Set.insert (Right g) seqs])
fourbox _ _ _ = []

removeBoxLeft :: Sequent FormM -> Sequent FormM
removeBoxLeft  = setComprehension isLeftBox fromBox

isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False
  
fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left (Box g)) = Left g
fromBox g = g