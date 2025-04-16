module IPL (intui) where

import Data.List as List
import qualified Data.Set as Set

import General
import Basics

intui :: Logic FormP
intui = Log
  { neg         = negP
  , bot         = BotP
  , isAtom      = isatomP
  , isAxiom     = isAxiomP
  , safeRule    = replaceRuleIPLsafe safeIPL
  , unsafeRules = [replaceRuleIPLunsafe unsafeIPL]
  }

-- | Safe rules
safeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeIPL (Left (ConP f g))  = [("L∧", [Set.insert (Left g)     $ Set.singleton (Left f)]  )]
safeIPL (Left (DisP f g))  = [("Lv", [Set.singleton (Left f)  , Set.singleton (Left g)]  )] -- branch
safeIPL (Right (ConP f g)) = [("R∧", [Set.singleton (Right f) , Set.singleton (Right g)] )] -- branch
safeIPL (Right (DisP f g)) = [("Rv", [Set.insert (Right g)    $ Set.singleton (Right f)] )]
safeIPL (Left (ImpP f g))  = [("L→", [Set.singleton (Left g)  , Set.singleton (Right f)] )] -- branch
safeIPL _                  = []

-- | The R-> rule.
unsafeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
unsafeIPL (Right (ImpP f g)) = [("R→", [Set.insert (Right g) $ Set.singleton (Left f)])]
unsafeIPL  _                 = []

-- | Is this sequent saturated?
saturated :: Sequent FormP -> Either FormP FormP -> Bool
saturated fs f@(Right (ImpP _ _)) = any (`Set.isSubsetOf` fs) (snd . head . unsafeIPL $ f)
saturated fs f = case safeIPL f of []              -> True
                                   ((_,results):_) -> any (`Set.isSubsetOf` fs) results

-- * IPL-specific versions of `replaceRule`.

-- | Like `replaceRule` but keeping the active formula (built-in weakening) and stopping when saturated.
replaceRuleIPLsafe :: (Either FormP FormP -> [(RuleName, [Sequent FormP])]) -> Rule FormP
replaceRuleIPLsafe fun _ fs g =
  [ ( fst . head $ fun g
    , [ fs `Set.union` newfs | newfs <- snd . head $ fun g] -- not deleting `g` here!
    )
  | not (saturated fs g) -- additional check
  , not (List.null (fun g)) ]

-- | Helper function for replaceRuleIPLunsafe. TODO: explain.
applyIPL :: Sequent FormP -> Either FormP FormP -> [Sequent FormP] -> [Sequent FormP]
applyIPL fs f = List.map (Set.insert f (leftOfSet fs) `Set.union`)

-- | Like `replaceRule` but ... TODO: explain.
replaceRuleIPLunsafe :: (Either FormP FormP -> [(RuleName, [Sequent FormP])]) -> Rule FormP
replaceRuleIPLunsafe fun h fs g =
  [ ( fst . head $ fun g
    , applyIPL fs g (snd (head (unsafeIPL g)))
    )
  | not (saturated fs g) -- additional check
  , historyCheck h fs g -- aditional check
  , not (List.null (fun g)) ]

-- | Check that the result of applying `unsafeIPL` to `f` does
-- not already occur (as a subset) in the history.
-- Helper function for `replaceRuleIPLunsafe`.
historyCheck :: [Sequent FormP] -> Sequent FormP -> Either FormP FormP -> Bool
historyCheck hs fs f@(Right (ImpP _ _)) =
  let xs = applyIPL fs f (snd (head (unsafeIPL f)))
  in not $ any (Set.isSubsetOf (head xs)) hs
historyCheck _ _ _ = False
