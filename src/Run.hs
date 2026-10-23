module Run (run) where

import Core
import Data.List (intercalate)
import Host (coreLeaves)
import Term

run :: Term -> [Term] -> String
run entry loaded = intercalate "\n" (map line (evaluate program))
  where
    program = Program [] (App (applicable entry) (chain (map snd coreLeaves ++ map applicable loaded)))
    line = \case
      Right (t, s) -> display s t
      Left (e, s) -> describe s e

applicable :: Term -> Term
applicable = \case
  rule@Rule {} -> chain [rule]
  other -> other
