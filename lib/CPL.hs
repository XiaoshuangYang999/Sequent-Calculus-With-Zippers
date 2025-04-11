module CPL where

import General
import qualified Data.Set as Set

classical :: Logic FormP
classical = Log
  { neg         = negP
  , bot         = BotP
  , isAtom      = isatomP
  , isAxiom     = isAxiomP
  , safeRule    = replaceRule safeCPL
  , unsafeRules = []
  }

safeCPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeCPL (Left (ConP f g))   = [("L∧", [Set.insert (Left g)     $ Set.singleton (Left f)]  )]
safeCPL (Left (DisP f g))   = [("Lv", [Set.singleton (Left f)  , Set.singleton (Left g)]  )]
safeCPL (Left (ImpP f g))   = [("L→", [Set.singleton (Left g)  , Set.singleton (Right f)] )]
safeCPL (Right (ConP f g))  = [("R∧", [Set.singleton (Right f) , Set.singleton (Right g)] )]
safeCPL (Right (DisP f g))  = [("Rv", [Set.insert (Right g)    $ Set.singleton (Right f)] )]
safeCPL (Right (ImpP f g))  = [("R→", [Set.insert (Right g)    $ Set.singleton (Left f)]  )]
safeCPL _                   = []
