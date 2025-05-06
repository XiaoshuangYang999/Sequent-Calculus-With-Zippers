module S4 (sfour) where

import qualified Data.Set as Set
import Data.List as List
import General
import K

sfour :: Logic FormM
sfour = Log { safeRules   = [leftBotM, isAxiomM, additionRule safeML, trule]
            , unsafeRules = [fourrule] }

-- | The T box rule. Involve local loopcheck
trule :: Rule FormM
trule _ fs (Left (Box f)) = [("☐T", [Set.insert (Left f) fs]) | Left f `notElem` fs]
trule _ _ _ = []

-- | The 4 box rule. Involve global loopcheck
fourrule :: Rule FormM
fourrule hs fs (Right (Box f)) = concatMap (globalLoopCheckMap hs) ss where
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
fourrule _ _ _ = []

-- Global loopcheck, seqs/= fs
globalLoopCheckMap :: History FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
globalLoopCheckMap hs seqs = [("☐4", [seqs]) | seqs `notElem` hs]

-- | Local loopcheck: Is this sequent saturated?
localLoopCheck :: Sequent FormM -> Either FormM FormM -> Bool
localLoopCheck fs f = case safeML f of []               -> False
                                       ((_,results):_)  -> not $ any (`Set.isSubsetOf` fs) results

additionRule :: (Either FormM FormM -> [(RuleName, [Sequent FormM])]) -> Rule FormM
additionRule fun _ fs g =
  [ ( fst . head $ fun g
    , [ fs `Set.union` newfs | newfs <- snd . head $ fun g ] -- not deleting `g` here!
    )
  | localLoopCheck fs g -- local loopcheck
  , not (List.null (fun g)) ]