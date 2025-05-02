module S4 where

import Basics
import General
import qualified Data.Set as Set

sfour :: Logic FormM
sfour = Log { safeRules    = [leftBotM, isAxiomM,replaceRule safeML]
            , unsafeRules = [fourrule,trule] }

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

-- | The T box rule.
trule :: Rule FormM
trule hs fs (Left (Box f)) = [("T", [Node (Set.insert (Left f) fs) Nothing]) | Set.insert (Left f) fs `notElem` hs]
trule _ _ _ = []

loopCheckMap :: History FormM -> Sequent FormM -> [(RuleName,[Proof FormM])]
loopCheckMap hs seqs = [("4", [Node seqs Nothing]) | seqs `notElem` hs]

removeBoxLeft :: Sequent FormM -> Sequent FormM
removeBoxLeft  = setComprehension isLeftBox fromBox

isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False

fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left (Box g)) = Left g
fromBox g = g
