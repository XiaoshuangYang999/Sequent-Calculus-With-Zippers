module IPL (intui) where

import General
import Basics
import qualified Data.Set as Set
import Data.List as List

intui :: Logic FormP
intui = Log
  { neg         = negP
  , bot         = BotP
  , isAtom      = isatomP
  , isAxiom     = isAxiomP
  , safeRule    = replaceRuleIPLsafe safeIPL
  , unsafeRules = [replaceRuleIPLunsafe unsafeIPL]
  }

-- Safe rules
safeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeIPL (Left (ConP f g))   = [("L∧", [Set.insert (Left g)     $ Set.singleton (Left f)]  )]
safeIPL (Left (DisP f g))   = [("Lv", [Set.singleton (Left f)  , Set.singleton (Left g)]  )] -- branch
safeIPL (Right (ConP f g))  = [("R∧", [Set.singleton (Right f) , Set.singleton (Right g)] )] -- branch
safeIPL (Right (DisP f g))  = [("Rv", [Set.insert (Right g)    $ Set.singleton (Right f)] )]
safeIPL (Left (ImpP f g))   = [("L→", [Set.singleton (Left g)  , Set.singleton (Right f)] )] -- branch
safeIPL _                   = []

-- R->
unsafeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
unsafeIPL (Right (ImpP f g)) = [("R→", [Set.insert (Right g) $ Set.singleton (Left f)])]
unsafeIPL  _                 = []

-- Return true iff saturated
saturated :: Sequent FormP -> Either FormP FormP -> Bool
saturated fs f@(Right (ImpP _ _)) =  any (`Set.isSubsetOf` fs) (snd . head . unsafeIPL $ f)
saturated fs f                    =  List.null (safeIPL f)
                                  || any (`Set.isSubsetOf` fs) (snd . head . safeIPL $ f)

-- TODO: remaining things here are not actually IPL specific, rename and move them to General?

historyCheck :: [Sequent FormP] -> Sequent FormP -> Either FormP FormP -> Bool
historyCheck hs fs f@(Right (ImpP _ _)) = not $ any (Set.isSubsetOf (head xs)) hs where
                                      xs = applyIPL fs f (snd(head(unsafeIPL f)))
historyCheck _ _ _ = False

-- TODO: compare these to standard `replaceRule` - factor our difference?

replaceRuleIPLsafe :: (Either FormP FormP -> [(RuleName,[Sequent FormP])]) -> Rule FormP
replaceRuleIPLsafe fun h fs g = [[(fst . head $ fun g
                                              ,[ fs `Set.union` newfs | newfs <- snd . head $ fun g])]
                                              | not (saturated fs g)
                                              && not (List.null (fun g))
                                              ]

-- Helper function for replaceRuleIPLunsafe
applyIPL :: Sequent FormP -> Either FormP FormP -> [Sequent FormP] -> [Sequent FormP]
applyIPL fs f = List.map (Set.insert f (leftOfSet fs) `Set.union`)

replaceRuleIPLunsafe :: (Either FormP FormP -> [(RuleName,[Sequent FormP])]) -> Rule FormP
replaceRuleIPLunsafe fun h fs g = [[(fst . head $ fun g
                                                , applyIPL fs g (snd(head(unsafeIPL g)))) ]
                                                | not (saturated fs g)
                                                && historyCheck h fs g
                                                && not (List.null (fun g))
                                                ]
