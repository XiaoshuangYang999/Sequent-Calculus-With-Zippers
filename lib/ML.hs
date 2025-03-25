module ML where

import General
import qualified Data.Set as Set

modal :: Logic FormM
modal = Log
  { neg         = negM
  , bot         = BotM
  , isAtom      = isatomM
  , isAxiom     = isAxiomM
  , safeRuleT   = replaceRuleT safeML
  , unsafeRuleT = [kboxT]  
  , safeRuleZ   = replaceRuleZ safeML
  , unsafeRuleZ = [kboxZ]
  }

-- Safe rules
safeML :: Either FormM FormM -> [(RuleName,[Sequent FormM])]
safeML (Left (ConM f g))  = [("L∧", [Set.insert (Left g)     $ Set.singleton (Left f)]  )]
safeML (Left (DisM f g))  = [("Lv", [Set.singleton (Left f)  , Set.singleton (Left g)]  )]
safeML (Left (ImpM f g))  = [("L→", [Set.singleton (Left g)  , Set.singleton (Right f)] )]
safeML (Right (ConM f g)) = [("R∧", [Set.singleton (Right f) , Set.singleton (Right g)] )]
safeML (Right (DisM f g)) = [("Rv", [Set.insert (Right g)    $ Set.singleton (Right f)] )]
safeML (Right (ImpM f g)) = [("R→", [Set.insert (Right g)    $ Set.singleton (Left f)]  )]
safeML _                  = []

-- Helper functions
isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False 

isRightBox :: Either FormM FormM -> Bool
isRightBox (Right (Box _)) = True
isRightBox _               = False 

fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left (Box f)) = Left f
fromBox g = g

removeBoxLeft :: Sequent FormM -> Sequent FormM 
removeBoxLeft xs = Set.map fromBox $ Set.filter isLeftBox xs

func :: FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
func f fs = [("K☐", [Right f `Set.insert` fs])]

-- K box
kboxT :: RuleT FormM
kboxT (_,Node fs "" []) (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet.removeBoxLeft $ fs
kboxT _ _ = []

kboxZ :: RuleZ FormM
kboxZ (ZP (Node fs "" []) _) (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet.removeBoxLeft $ fs
kboxZ _ _ = []