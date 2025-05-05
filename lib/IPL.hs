module IPL (intui) where

import Data.List as List
import qualified Data.Set as Set

import General
import Basics

intui :: Logic FormP
intui = Log { safeRules   = [leftBotP, isAxiomP, additionRule safeIPL]
            , unsafeRules = [additionRuleNoLoop unsafeIPL] }

-- | Safe rules
safeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeIPL (Left (ConP f g))  = [("∧L", [Set.fromList [Left g, Left f]])]
safeIPL (Left (DisP f g))  = [("vL", map Set.singleton [Left f, Left g])]
safeIPL (Left (ImpP f g))  = [("→L", map Set.singleton [Left g, Right f])]
safeIPL (Right (ConP f g)) = [("∧R", map Set.singleton [Right f, Right g])]
safeIPL (Right (DisP f g)) = [("vR", [Set.fromList [Right g, Right f]])]
safeIPL _                  = []

-- TODO: where is the →iL rule?

-- | The R-> rule.
unsafeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
unsafeIPL (Right (ImpP f g)) = [("→iR", [Set.fromList [Right g, Left f]])]
unsafeIPL  _                 = []

-- | Is this sequent saturated?
saturated :: Sequent FormP -> Either FormP FormP -> Bool
saturated fs f@(Right (ImpP _ _)) = any (`Set.isSubsetOf` fs) (snd . head . unsafeIPL $ f)
saturated fs f = case safeIPL f of []              -> True
                                   ((_,results):_) -> any (`Set.isSubsetOf` fs) results

-- * IPL-specific versions of `replaceRule`.

-- | Like `replaceRule` but keep active formula (built-in weakening), and block when saturated.
additionRule :: (Either FormP FormP -> [(RuleName, [Sequent FormP])]) -> Rule FormP
additionRule fun _ fs g =
  [ ( fst . head $ fun g
    , [ fs `Set.union` newfs | newfs <- snd . head $ fun g ] -- not deleting `g` here!
    )
  | not (saturated fs g) -- local loopcheck
  , not (List.null (fun g)) ]

-- | Helper function for replaceRuleIPLunsafe.
applyIPL :: Sequent FormP -> Either FormP FormP -> [Sequent FormP] -> [Sequent FormP]
applyIPL fs f = List.map (Set.insert f (leftOfSet fs) `Set.union`)

-- | Like `additionRule` but also doing a global loopcheck.
additionRuleNoLoop :: (Either FormP FormP -> [(RuleName, [Sequent FormP])]) -> Rule FormP
additionRuleNoLoop fun h fs g =
  [ ( fst . head $ fun g
    , applyIPL fs g $ snd . head . fun $ g
    )
  | not (saturated fs g) -- local loopcheck
  , historyCheck h fs g -- gobal loopcheck
  , not (List.null (fun g)) ]

-- | Check that the result of applying `unsafeIPL` to `f` does
-- not already occur (as a subset) in the history.
-- Helper function for `replaceRuleIPLunsafe`.
historyCheck :: [Sequent FormP] -> Sequent FormP -> Either FormP FormP -> Bool
historyCheck hs fs f@(Right (ImpP _ _)) =
  let xs = applyIPL fs f (snd (head (unsafeIPL f)))
  in not $ any (Set.isSubsetOf (head xs)) hs
historyCheck _ _ _ = False
