module Format (format) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Parse

width :: Int
width = 120

format :: FilePath -> Text -> Either String String
format path input = do
  let (header, body) = span isHeader (T.lines input)
      bodyText = T.unlines body
  when ("--" `T.isInfixOf` bodyText) (Left (path ++ ": comments are only kept at the top of the file"))
  raw <- readRaw path bodyText
  let comments = [T.unpack l | l <- header, not (T.null (T.strip l))]
  pure (concatMap (++ "\n") comments ++ (if null comments then "" else "\n") ++ layout 0 0 raw)
  where
    isHeader l = T.null (T.strip l) || "--" `T.isPrefixOf` T.stripStart l

flat :: Raw -> Maybe String
flat = flatWith True

flatLeft :: Raw -> Maybe String
flatLeft = flatWith False

flatWith :: Bool -> Raw -> Maybe String
flatWith breaking = \case
  RVar v -> Just v
  RPin v -> Just ('^' : v)
  RFree v -> Just ('#' : v)
  RNew s -> Just ('$' : s)
  RLiteral _ s -> Just s
  RApp f a -> (\f' a' -> "(" ++ f' ++ " " ++ a' ++ ")") <$> flatOperand f <*> argument a
  RRule {} -> Nothing
  RPipe start steps -> do
    start' <- flatOperand start
    steps' <- mapM flatStep steps
    pure (unwords (start' : steps'))
  RChain items rest
    | breaking && length items > 2 && not (all atom items) -> Nothing
    | otherwise -> do
        items' <- mapM (flatWith breaking) items
        rest' <- traverse (flatWith breaking) rest
        pure ("[" ++ unwords items' ++ maybe "" (" | " ++) rest' ++ "]")
  RScope {} -> Nothing
  where
    argument = \case
      a@RChain {} -> flatLeft a
      a -> flatOperand a
    atom = \case
      RApp {} -> False
      RChain {} -> False
      RRule {} -> False
      RPipe {} -> False
      RScope {} -> False
      _ -> True

flatStep :: Step -> Maybe String
flatStep = \case
  Feed f -> ("|> " ++) <$> flatOperand f
  Into _ -> Nothing

wrapped :: Raw -> Bool
wrapped = \case
  RRule {} -> True
  RPipe {} -> True
  _ -> False

flatOperand :: Raw -> Maybe String
flatOperand raw
  | wrapped raw = (\s -> "(" ++ s ++ ")") <$> flat raw
  | otherwise = flat raw

operand :: Int -> Int -> Raw -> String
operand indent column raw
  | wrapped raw = "(\n" ++ pad ++ layout (indent + 2) (indent + 2) raw ++ "\n" ++ replicate indent ' ' ++ ")"
  | otherwise = layout indent column raw
  where
    pad = replicate (indent + 2) ' '

body :: Int -> Int -> Raw -> String
body indent column raw = case raw of
  RRule {} -> operand indent column raw
  _ -> layout indent column raw

layout :: Int -> Int -> Raw -> String
layout indent column raw = case flat raw of
  Just s | column + length s <= width -> s
  _ -> case raw of
    RChain items rest ->
      "[\n"
        ++ concatMap (\x -> pad inner ++ layout inner inner x ++ "\n") items
        ++ maybe "" (\t -> pad inner ++ "| " ++ layout inner (inner + 2) t ++ "\n") rest
        ++ pad indent
        ++ "]"
    RRule Forward l r -> leftSide column l ++ " :>" ++ ruleBody r
    RRule Reversed l r -> body indent column r ++ "\n" ++ pad inner ++ "<: " ++ leftSide (inner + 3) l
    RPipe start steps -> operand indent column start ++ concatMap ((" |> " ++) . step) steps
    RApp f a | Just head <- flatOperand f, column + length head + 2 <= width -> "(" ++ head ++ " " ++ argument (column + length head + 2) a ++ ")"
    RApp f a -> "(\n" ++ pad inner ++ operand inner inner f ++ "\n" ++ pad inner ++ operand inner inner a ++ "\n" ++ pad indent ++ ")"
    RScope bindings g result -> concat (zipWith3 piece [0 :: Int ..] (map gap bindings ++ [g]) (map line bindings ++ [body indent indent result]))
    atom -> maybe "" id (flat atom)
  where
    inner = indent + 2
    pad n = replicate n ' '
    leftSide col l = case flatLeft l of
      Just s | col + length s <= width -> s
      _ -> layout indent col l
    argument col a = case a of
      RChain {} -> leftSide col a
      _ -> operand indent col a
    ruleBody r = case r of
      RChain {} | maybe True (\s -> inner + length s > width) (flat r) -> " " ++ layout indent inner r
      _ -> "\n" ++ pad inner ++ body inner inner r
    step = \case
      Feed f -> operand indent inner f
      Into r -> layout indent inner r
    line b = let left = maybe "" id (flatLeft (pattern b)) in left ++ " := " ++ body indent (indent + length left + 4) (value b)
    piece i g s = (if i == 0 then "" else "\n" ++ (if g then "\n" else "") ++ pad indent) ++ s
