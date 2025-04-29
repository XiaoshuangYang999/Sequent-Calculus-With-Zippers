module K4 where

import General
import qualified Data.Set as Set

kfour :: Logic FormM
kfour = Log
  { neg         = negM
  , bot         = BotM
  , isAtom      = isatomM
  , isAxiom     = isAxiomM
  , safeRule    = replaceRule safeML
  , unsafeRules = [fourrule]
  , allowCycle = False
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

-- | The 4 box rule.
fourrule :: Rule FormM
fourrule hs fs (Right (Box f)) = concatMap (loopCheckMap hs) ss where
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
fourrule _ _ _ = [] 

loopCheckMap :: History FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
loopCheckMap hs seqs = [("4", [seqs]) | seqs `notElem` hs]

isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False

fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left (Box g)) = Left g
fromBox g = g
