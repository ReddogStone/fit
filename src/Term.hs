module Term where

import Data.Char (isDigit)

type Name = String

data Term
  = Sym Name
  | Var Int Int
  | Meta Int
  | App Term Term
  | Rule [Name] Term Term
  | Alt [Term]
  | Cons Term Term
  | Nil
  deriving (Eq, Ord, Show)

data Program = Program
  { unknowns :: [Name],
    middle :: Term
  }

hostSym :: String -> Name
hostSym = ('#' :)

numberName, stringName :: Name
numberName = hostSym "number"
stringName = hostSym "string"

shown :: Name -> String
shown = takeWhile (/= '#') . dropWhile (== '#')

chain :: [Term] -> Term
chain = foldr Cons Nil

unchain :: Term -> ([Term], Maybe Term)
unchain = \case
  Nil -> ([], Nothing)
  Cons x rest -> let (xs, tail') = unchain rest in (x : xs, tail')
  t -> ([], Just t)

isLiteral :: String -> Bool
isLiteral = \case
  '"' : _ -> True
  '-' : c : _ -> isDigit c
  c : _ -> isDigit c
  [] -> False

pretty :: (Int -> String) -> Term -> String
pretty namer = go []
  where
    go stack = \case
      Sym s -> shown s
      Var d k -> case drop d stack of
        (frame : _) | k < length frame -> shown (frame !! k)
        _ -> "v" ++ show k ++ "^" ++ show d
      Meta m -> namer m
      App (Sym c) (Sym s) | c == numberName || c == stringName, isLiteral s -> s
      App f a -> "(" ++ go stack f ++ " " ++ go stack a ++ ")"
      Rule ns l r -> "(" ++ go (ns : stack) l ++ " :> " ++ go (ns : stack) r ++ ")"
      Alt items -> "<" ++ unwords (map (go stack) items) ++ ">"
      t -> case unchain t of
        (items, Nothing) -> "[" ++ unwords (map (go stack) items) ++ "]"
        (items, Just rest) -> "[" ++ unwords (map (go stack) items) ++ " | " ++ go stack rest ++ "]"
