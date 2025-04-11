module ML (modal) where

import General
import qualified Data.Set as Set

modal :: Logic FormM
modal = Log
  { neg         = negM
  , bot         = BotM
  , isAtom      = isatomM
  , isAxiom     = isAxiomM
  , safeRule    = replaceRule safeML
  , unsafeRules = [kbox]
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
kbox _ fs (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet . removeBoxLeft $ fs where
  removeBoxLeft :: Sequent FormM -> Sequent FormM
  removeBoxLeft xs = Set.map fromBox $ Set.filter isLeftBox xs
  isLeftBox :: Either FormM FormM -> Bool
  isLeftBox (Left (Box _)) = True
  isLeftBox _              = False
  fromBox :: Either FormM FormM -> Either FormM FormM
  fromBox (Left (Box f)) = Left f
  fromBox g = g
  func :: FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
  func g seqs = [("K☐", [Set.insert (Right g) seqs])]
kbox _ _ _ = []
