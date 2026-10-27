module Host where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Term
import Text.Read (readMaybe)

data HostType = Number | String deriving (Eq, Show, Read)

data Shape = One HostType | Many [HostType]

data HostFn = HostFn [Shape] ([Term] -> Maybe Term)

anyName :: Name
anyName = hostSym "any"

anyOf :: HostType -> Term
anyOf ty = App (Sym anyName) (Sym (show ty))

anyType :: Term -> Maybe HostType
anyType = \case
  App (Sym a) (Sym ty) | a == anyName -> readMaybe ty
  _ -> Nothing

solveName :: Name
solveName = hostSym "solve"

resumeName :: Name
resumeName = hostSym "resume"

hostTypeOf :: Term -> Maybe HostType
hostTypeOf = \case
  Sym ('"' : _) -> Just String
  Sym s | Just (_ :: Integer) <- readMaybe s -> Just Number
  _ -> Nothing

asNumber :: Term -> Maybe Integer
asNumber = \case
  Sym s -> readMaybe s
  _ -> Nothing

asString :: Term -> Maybe [Char]
asString = \case
  Sym s -> readMaybe s
  _ -> Nothing

numberSym, stringSym, boolSym :: Term
numberSym = Sym numberName
stringSym = Sym stringName
boolSym = Sym (hostSym "bool")

numberTerm :: Integer -> Term
numberTerm = App numberSym . Sym . show

stringTerm :: [Char] -> Term
stringTerm = App stringSym . Sym . show

bool :: Bool -> Term
bool b = Sym (hostSym (if b then "true" else "false"))

valueHost :: Map Name HostFn
valueHost =
  Map.fromList
    [ (hostSym "add", numbers (+)),
      (hostSym "sub", numbers (-)),
      (hostSym "mul", numbers (*)),
      (hostSym "lt", compare' (<)),
      (hostSym "le", compare' (<=)),
      (hostSym "gt", compare' (>)),
      (hostSym "ge", compare' (>=)),
      (hostSym "eq", HostFn [Many [Number, Number], Many [String, String]] (\case [a, b] -> Just (bool (a == b)); _ -> Nothing)),
      (hostSym "concat", HostFn [Many [String, String]] (\case [a, b] -> stringTerm <$> ((++) <$> asString a <*> asString b); _ -> Nothing)),
      (hostSym "show", HostFn [Many [Number]] (\case [a] -> stringTerm . show <$> asNumber a; _ -> Nothing))
    ]
  where
    numbers op = HostFn [Many [Number, Number]] (\case [a, b] -> numberTerm <$> (op <$> asNumber a <*> asNumber b); _ -> Nothing)
    compare' op = HostFn [Many [Number, Number]] (\case [a, b] -> bool <$> (op <$> asNumber a <*> asNumber b); _ -> Nothing)

literalLeaf :: HostType -> Name
literalLeaf = \case
  Number -> "number"
  String -> "string"

foreignName :: Name
foreignName = hostSym "foreign"

tieName :: Name
tieName = hostSym "tie"

coreLeaves :: [(Name, Term)]
coreLeaves = [("tie", Sym tieName), ("foreign", Sym foreignName), ("solve", Sym solveName)]

hostTable :: Map Name Term
hostTable =
  Map.fromList
    ( [ ("number", numberSym),
        ("string", stringSym),
        ("bool", boolSym),
        ("true", bool True),
        ("false", bool False)
      ]
        ++ [(shown n, Sym n) | n <- Map.keys valueHost]
    )
