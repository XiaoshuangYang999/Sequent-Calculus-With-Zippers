module K4box where

import General
import Basics
import qualified Data.Set as Set

kfourbox :: Logic FormM
kfourbox = Log
  { neg         = negM
  , bot         = BotM
  , isAtom      = isatomM
  , isAxiom     = isAxiomM
  , safeRule    = replaceRule safeML
  , unsafeRules = [fourbox]
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








-- xs, box xs => ys 
-- fs, box xs => box ys, gs
-- | The 4 box rule.
fourbox :: Rule FormM
fourbox hs fs (Right (Box f)) = Set.toList $
  setComprehension (`notElem` hs) func $
    Set.map (Set.insert (Right f)) $ 
      Set.powerSet . extractBoxLeft $ fs
  where
  extractBoxLeft :: Sequent FormM -> Sequent FormM -- find a better name
  extractBoxLeft xs = Set.unions $ Set.map (\g -> Set.fromList [g,fromBox g]) $ Set.filter isLeftBox xs
  isLeftBox :: Either FormM FormM -> Bool
  isLeftBox (Left (Box _)) = True
  isLeftBox _              = False
  fromBox :: Either FormM FormM -> Either FormM FormM
  fromBox (Left (Box g)) = Left g
  fromBox g = g
  func :: Sequent FormM -> (RuleName,[Sequent FormM])
  func seqs = ("4☐", [seqs])
fourbox _ _ _ = []

-- ToDo: add loop check