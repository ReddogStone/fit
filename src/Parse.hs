module Parse (parseFile, readRaw, Raw (..), Arrow (..), Step (..), Binding (..), Joiner (..)) where

import Control.Monad (forM_, void, when)
import Data.Char (isLower, isSpace, isUpper, toLower)
import Data.List (elemIndex, intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Host (HostType (..), literalLeaf)
import Term
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Raw
  = RVar Name
  | RPin Name
  | RFree Name
  | RNew Name
  | RLiteral HostType String
  | RApp Raw Raw
  | RRule Arrow Raw Raw
  | RChain [Raw] (Maybe Raw)
  | RAlt [Raw]
  | RScope [Binding] Bool Raw
  | RPipe Raw [Step]

data Arrow = Forward | Reversed deriving (Eq)

data Step = Feed Raw | Into Raw

stepRaw :: Step -> Raw
stepRaw = \case
  Feed f -> f
  Into r -> r

mapStep :: (Raw -> Raw) -> Step -> Step
mapStep f = \case
  Feed t -> Feed (f t)
  Into r -> Into (f r)

desugar :: Raw -> [Step] -> Raw
desugar = foldl (\arg step -> RApp (applicable (stepRaw step)) arg)

applicable :: Raw -> Raw
applicable = \case
  rule@RRule {} -> RAlt [rule]
  other -> other

data Joiner = Let | Bind deriving (Eq)

data Binding = Binding {pattern :: Raw, joiner :: Joiner, gap :: Bool, value :: Raw}

parseFile :: FilePath -> Text -> Either String Term
parseFile path input = do
  raw <- lower <$> readRaw path input
  case raw of
    RRule {} -> pure ()
    _ -> Left (path ++ ": a file is a rule, inputs :> body")
  checkSides raw
  checkCreations raw
  lint raw
  convert (Env [] Map.empty) raw

readRaw :: FilePath -> Text -> Either String Raw
readRaw path = first . parse (space' *> (term >>= \t -> ruled t <|> pure t) <* eof) path

first :: Either (ParseErrorBundle Text Void) a -> Either String a
first = either (Left . errorBundlePretty) Right

lower :: Raw -> Raw
lower = \case
  RVar v -> RVar (map toLower v)
  RPin v -> RPin (map toLower v)
  RFree v -> RFree (map toLower v)
  RNew s -> RNew (map toLower s)
  RApp f a -> RApp (lower f) (lower a)
  RRule d l r -> RRule d (lower l) (lower r)
  RChain items rest -> RChain (map lower items) (lower <$> rest)
  RAlt items -> RAlt (map lower items)
  RScope bindings g result -> RScope [b {pattern = lower (pattern b), value = lower (value b)} | b <- bindings] g (lower result)
  RPipe start steps -> RPipe (lower start) (map (mapStep lower) steps)
  literal -> literal

space' :: Parser ()
space' = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space'

symbol :: Text -> Parser ()
symbol = void . L.symbol space'

identifier :: Parser Name
identifier = lexeme $ do
  c <- satisfy (\c -> isUpper c || isLower c)
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure (c : rest)

binding :: Parser Binding
binding = Binding <$> atom <*> (Let <$ symbol ":=" <|> Bind <$ symbol "<-") <*> pure False <*> pipe

term :: Parser Raw
term = do
  start <- getOffset
  rest <- getInput
  let gapBefore o = T.count "\n" (T.takeWhileEnd isSpace (T.take (o - start) rest)) > 1
  bindings <- many (try ((\o b -> b {gap = gapBefore o}) <$> getOffset <*> binding))
  o <- getOffset
  result <- pipe
  pure (if null bindings then result else RScope bindings (gapBefore o) result)

pipe :: Parser Raw
pipe = do
  start <- atom
  steps <- many (symbol "|>" *> atom >>= \t -> Into <$> ruled t <|> pure (Feed t))
  pure (if null steps then start else RPipe start steps)

atom :: Parser Raw
atom =
  choice
    [ RVar <$> identifier,
      RPin <$> (char '^' *> identifier),
      RFree <$> (char '#' *> identifier),
      RNew <$> (char '$' *> identifier),
      RLiteral Number . show <$> lexeme (L.signed (pure ()) L.decimal :: Parser Integer),
      RLiteral String . show <$> lexeme (char '"' *> manyTill L.charLiteral (char '"')),
      between (symbol "(") (symbol ")") (term >>= \t -> ruled t <|> RApp t <$> term <|> grouped t),
      between (symbol "[") (symbol "]") chainBody,
      between opener (symbol ">") (RAlt <$> items)
    ]
  where
    opener = lexeme (try (char '<' <* notFollowedBy (oneOf ("-:" :: String))))

ruled :: Raw -> Parser Raw
ruled t =
  choice
    [ symbol ":>" *> (RRule Forward t <$> term),
      symbol "<:" *> ((\l -> RRule Reversed l t) <$> atom)
    ]

grouped :: Raw -> Parser Raw
grouped = \case
  t@RPipe {} -> pure t
  _ -> empty

chainBody :: Parser Raw
chainBody = RChain <$> items <*> optional (symbol "|" *> atom)

items :: Parser [Raw]
items = many (term >>= \t -> ruled t <|> pure t)

checkSides :: Raw -> Either String ()
checkSides = \case
  RRule _ l r -> left l >> checkSides r
  RApp f a -> checkSides f >> checkSides a
  RChain items rest -> mapM_ checkSides items >> maybe (pure ()) checkSides rest
  RAlt items -> mapM_ checkSides items
  RScope bindings _ result -> mapM_ (\b -> left (pattern b) >> checkSides (value b)) bindings >> checkSides result
  RPipe start steps -> mapM_ checkSides (start : map stepRaw steps)
  RPin v -> Left ("^" ++ v ++ " outside a left side")
  _ -> pure ()
  where
    left = \case
      RRule {} -> Left "a rule inside a left side"
      RAlt {} -> Left "alternatives inside a left side"
      RScope {} -> Left "a binding inside a left side"
      RPipe {} -> Left "a pipe inside a left side"
      RNew s -> Left ("$" ++ s ++ " inside a left side")
      RFree s -> Left ("#" ++ s ++ " inside a left side")
      RApp f a -> left f >> left a
      RChain items rest -> mapM_ left items >> maybe (pure ()) left rest
      _ -> pure ()

checkCreations :: Raw -> Either String ()
checkCreations raw = forM_ (nub (creations raw)) $ \s ->
  when (length (filter (== s) (creations raw)) > 1) (Left ("$" ++ s ++ " is created more than once"))
  where
    creations = \case
      RNew s -> [s]
      RApp f a -> creations f ++ creations a
      RRule _ l r -> creations l ++ creations r
      RChain items rest -> concatMap creations items ++ maybe [] creations rest
      RAlt items -> concatMap creations items
      RScope bindings _ result -> concatMap (creations . value) bindings ++ creations result
      RPipe start steps -> concatMap creations (start : map stepRaw steps)
      _ -> []

lint :: Raw -> Either String ()
lint = \case
  RRule _ l r -> lint l >> lint r
  RApp f a -> lint f >> lint a
  RAlt items -> do
    let lefts = [l | RRule _ l _ <- items]
    when (length lefts == length items) (mapM_ apartRaw (pairs lefts))
    mapM_ lint items
  RChain items rest -> mapM_ lint items >> maybe (pure ()) lint rest
  RScope bindings _ result -> mapM_ (lint . value) bindings >> lint result
  RPipe start steps -> mapM_ lint (start : map stepRaw steps)
  _ -> pure ()
  where
    pairs = \case
      [] -> []
      x : rest -> [(x, y) | y <- rest] ++ pairs rest

apartRaw :: (Raw, Raw) -> Either String ()
apartRaw (p, q) = if go (cons p) (cons q) then pure () else Left ("two alternatives overlap: " ++ showRaw p ++ " and " ++ showRaw q)
  where
    go a b = case (a, b) of
      (RVar _, _) -> False
      (_, RVar _) -> False
      (RPin v, RPin w) -> v /= w
      (RLiteral _ x, RLiteral _ y) -> x /= y
      (RNew x, RNew y) -> x /= y
      (RApp f x, RApp g y) -> go f g || go x y
      _ -> True
    cons = \case
      RChain items rest -> foldr (\x r -> RApp (RApp (RNew "[|]") (cons x)) r) (maybe (RNew "[]") cons rest) items
      RAlt _ -> RNew "<>"
      RApp f a -> RApp (cons f) (cons a)
      t -> t

showRaw :: Raw -> String
showRaw = \case
  RVar v -> v
  RPin v -> "^" ++ v
  RFree v -> "#" ++ v
  RNew s -> "$" ++ s
  RLiteral _ s -> s
  RApp f a -> "(" ++ showRaw f ++ " " ++ showRaw a ++ ")"
  RRule _ l r -> showRaw l ++ " :> " ++ showRaw r
  RChain items rest -> "[" ++ joinWith " " (map showRaw items) ++ maybe "" ((" | " ++) . showRaw) rest ++ "]"
  RAlt items -> "<" ++ joinWith " " (map showRaw items) ++ ">"
  RScope _ _ r -> showRaw r
  RPipe start steps -> showRaw (desugar start steps)
  where
    joinWith sep = \case
      [] -> ""
      xs -> foldr1 (\x acc -> x ++ sep ++ acc) xs

holesOf :: Raw -> [Name]
holesOf = nub . go
  where
    go = \case
      RVar v -> [v]
      RApp f a -> go f ++ go a
      RChain items rest -> concatMap go items ++ maybe [] go rest
      RAlt items -> concatMap go items
      _ -> []

freesOf :: Raw -> [Name]
freesOf = nub . go
  where
    go = \case
      RFree v -> [v]
      RApp f a -> go f ++ go a
      RChain items rest -> concatMap go items ++ maybe [] go rest
      RAlt items -> concatMap go items
      RScope bindings _ result -> concatMap (go . value) bindings ++ go result
      RPipe start steps -> concatMap go (start : map stepRaw steps)
      _ -> []

knotName :: Name -> Name
knotName n = n ++ "#knot"

underRules :: Name -> (Raw, Raw) -> Raw -> (Bool, Raw)
underRules n (forVar, forPin) = outside
  where
    outside = \case
      RApp f a -> both RApp (outside f) (outside a)
      RRule d l r -> both (RRule d) (inside l) (inside r)
      RChain items rest -> chain' outside items rest
      RAlt items -> alt' outside items
      RScope bindings g result -> let (p, bs, r) = scope outside bindings result in (p, RScope bs g r)
      RPipe start steps -> pipe' outside start steps
      t -> (False, t)
    inside = \case
      RVar v | v == n -> (True, forVar)
      RPin v | v == n -> (True, forPin)
      RApp f a -> both RApp (inside f) (inside a)
      RRule d l r -> both (RRule d) (inside l) (inside r)
      RChain items rest -> chain' inside items rest
      RAlt items -> alt' inside items
      RScope bindings g result -> let (p, bs, r) = scope inside bindings result in (p, RScope bs g r)
      RPipe start steps -> pipe' inside start steps
      t -> (False, t)
    both c (p, x) (q, y) = (p || q, c x y)
    alt' go items = let (ps, items') = unzip (map go items) in (or ps, RAlt items')
    pipe' go start steps =
      let (p, start') = go start
          (ps, steps') = unzip [let (q, t') = go (stepRaw s) in (q, mapStep (const t') s) | s <- steps]
       in (p || or ps, RPipe start' steps')
    chain' go items rest =
      let (ps, items') = unzip (map go items)
          (q, rest') = maybe (False, Nothing) (fmap Just . go) rest
       in (or ps || q, RChain items' rest')
    scope go bindings result = case bindings of
      [] -> let (p, r) = go result in (p, [], r)
      b : rest ->
        let (p, v) = go (value b)
            (q, rest', result') = if n `elem` holesOf (pattern b) then (False, rest, result) else scope go rest result
         in (p || q, b {value = v} : rest', result')

data Env = Env
  { frames :: [[Name]],
    unknownIndex :: Map.Map Name Int
  }

convert :: Env -> Raw -> Either String Term
convert env = \case
  RVar v -> resolve v
  RPin v -> resolve v
  RFree v -> resolve v
  RNew s -> pure (Sym s)
  RLiteral ty s -> (`App` Sym s) <$> resolve (literalLeaf ty)
  RApp f a -> App <$> go (applicable f) <*> go a
  RRule _ l r -> do
    let frame = holesOf l ++ filter (`notElem` holesOf l) (freesOf r)
        inner = env {frames = frame : frames env}
    Rule frame <$> convert inner l <*> convert inner r
  RChain items rest -> case rest of
    Nothing -> chain <$> mapM go items
    Just tail' -> foldr Cons <$> go tail' <*> mapM go items
  RAlt items -> Alt <$> mapM go items
  RPipe start steps -> go (desugar start steps)
  RScope [] _ result -> go result
  RScope (b : rest) g result -> do
    let holes = holesOf (pattern b)
        bound = env {frames = holes : frames env}
        self = knotName (intercalate "#" holes)
        stand n = case pattern b of
          RVar _ -> (RVar self, RPin self)
          shape -> let project ref = RApp (RAlt [RRule Forward shape (RVar n)]) ref in (project (RVar self), project (RPin self))
        (tied, renamed) = foldr (\n (p, raw) -> let (q, raw') = underRules n (stand n) raw in (p || q, raw')) (False, value b) holes
    inner <- convert bound (RScope rest g result)
    valueTerm <-
      if tied
        then do
          fix <- either (const (Left "a recursive line needs fix in scope")) Right (go (RVar "fix"))
          App fix . Alt . (: []) . Rule [self] (Var 0 0) <$> convert env {frames = [self] : frames env} renamed
        else go (value b)
    line <- Alt . (: []) <$> (Rule holes <$> convert bound (pattern b) <*> pure inner)
    case joiner b of
      Let -> pure (App line valueTerm)
      Bind -> do
        bind <- either (const (Left "a bind line needs bind in scope")) Right (go (RVar "bind"))
        pure (App bind (chain [valueTerm, line]))
  where
    go = convert env
    resolve v = case lookupVar 0 (frames env) v of
      Just t -> Right t
      Nothing
        | Just k <- Map.lookup v (unknownIndex env) -> Right (Meta k)
        | otherwise -> Left ("unbound name: " ++ v)

lookupVar :: Int -> [[Name]] -> Name -> Maybe Term
lookupVar _ [] _ = Nothing
lookupVar depth (frame : outer) v = case elemIndex v frame of
  Just k -> Just (Var depth k)
  Nothing -> lookupVar (depth + 1) outer v
