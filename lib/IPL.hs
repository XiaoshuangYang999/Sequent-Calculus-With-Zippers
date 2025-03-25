module IPL where

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
  , safeRuleT   = replaceRuleIPLsafeT safeIPL
  , unsafeRuleT = [replaceRuleIPLunsafeT unsafeIPL]
  , safeRuleZ   = replaceRuleIPLsafeZ safeIPL
  , unsafeRuleZ = [replaceRuleIPLunsafeZ unsafeIPL]
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

-- Return true iff has appeared before
historySearch :: ZipPath FormP -> Sequent FormP -> Bool
historySearch Top _ = False
historySearch (Step xs _ p _ _) ys = Set.isSubsetOf ys xs || historySearch p ys

-- Return true iff doesn't result in loop
historyCheckZ :: ZipPath FormP -> Sequent FormP -> Either FormP FormP -> Bool
historyCheckZ p fs f@(Right (ImpP _ _)) = not (historySearch p (head xs)) where
                                     xs = applyIPL fs f (snd(head(unsafeIPL f)))
historyCheckZ _ _ _ = False

historyCheckT :: [Sequent FormP] -> Sequent FormP -> Either FormP FormP -> Bool
historyCheckT hs fs f@(Right (ImpP _ _)) = not $ any (Set.isSubsetOf (head xs)) hs where
                                      xs = applyIPL fs f (snd(head(unsafeIPL f)))
historyCheckT _ _ _ = False


replaceRuleIPLsafeZ :: (Either FormP FormP -> [(RuleName,[Sequent FormP])]) -> RuleZ FormP
replaceRuleIPLsafeZ fun (ZP (Node fs "" []) _) g = [[(fst . head $ fun g
                                                   ,[ fs `Set.union` newfs | newfs <- snd . head $ fun g])] 
                                                   | not (saturated fs g) 
                                                   && not (List.null (fun g))
                                                   ]
replaceRuleIPLsafeZ _ _ _ = []

replaceRuleIPLsafeT :: (Either FormP FormP -> [(RuleName,[Sequent FormP])]) -> RuleT FormP
replaceRuleIPLsafeT fun (_,Node fs "" []) g = [[(fst . head $ fun g
                                              ,[ fs `Set.union` newfs | newfs <- snd . head $ fun g])] 
                                              | not (saturated fs g) 
                                              && not (List.null (fun g))
                                              ]
replaceRuleIPLsafeT _ _ _ = []

-- Helper function for replaceRuleIPLunsafe
applyIPL :: Sequent FormP -> Either FormP FormP -> [Sequent FormP] -> [Sequent FormP]
applyIPL fs f = List.map (Set.insert f (leftOfSet fs) `Set.union`)

replaceRuleIPLunsafeZ :: (Either FormP FormP -> [(RuleName,[Sequent FormP])]) -> RuleZ FormP
replaceRuleIPLunsafeZ fun (ZP (Node fs "" []) p) g = [[(fst . head $ fun g
                                                     , applyIPL fs g (snd(head(unsafeIPL g))))] 
                                                     | not (saturated fs g) 
                                                     && historyCheckZ p fs g 
                                                     && not (List.null (fun g))  
                                                     ]
replaceRuleIPLunsafeZ _ _ _ = []

replaceRuleIPLunsafeT :: (Either FormP FormP -> [(RuleName,[Sequent FormP])]) -> RuleT FormP
replaceRuleIPLunsafeT fun (h,Node fs "" []) g = [[(fst . head $ fun g
                                                , applyIPL fs g (snd(head(unsafeIPL g)))) ] 
                                                | not (saturated fs g) 
                                                && historyCheckT h fs g 
                                                && not (List.null (fun g))  
                                                ]
replaceRuleIPLunsafeT _ _ _ = []