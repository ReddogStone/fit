module Core
  ( S,
    Err (..),
    fatal,
    evaluate,
    display,
    describe,
  )
where

import Control.Monad (filterM, forM_, when, zipWithM_)
import Data.Either (isRight)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Host
import Term

data Err
  = NoRule Term
  | Mismatch Term Term
  | Occurs Int Term
  | Unbounded Term
  | NotARule Term
  deriving (Show)

fatal :: Err -> Bool
fatal = \case
  NoRule _ -> False
  Mismatch _ _ -> False
  Occurs _ _ -> False
  _ -> True

data S = S
  { counter :: Int,
    subst :: Map Int Term,
    names :: Map Int Name,
    active :: [Term]
  }

initial :: Program -> S
initial p = S (length (unknowns p)) Map.empty (Map.fromList (zip [0 ..] (unknowns p))) []

newtype M a = M {runM :: S -> [Either (Err, S) (a, S)]}

instance Functor M where
  fmap f (M m) = M (map (fmap (\(a, s) -> (f a, s))) . m)

instance Applicative M where
  pure a = M (\s -> [Right (a, s)])
  mf <*> ma = mf >>= \f -> fmap f ma

instance Monad M where
  M m >>= f = M $ \s -> concatMap (either (\e -> [Left e]) (\(a, s') -> runM (f a) s')) (m s)

get' :: M S
get' = M (\s -> [Right (s, s)])

put' :: S -> M ()
put' s = M (const [Right ((), s)])

modify' :: (S -> S) -> M ()
modify' f = get' >>= put' . f

gets' :: (S -> a) -> M a
gets' f = f <$> get'

throw :: Err -> M a
throw e = M (\s -> [Left (e, s)])

branch :: [M a] -> M a
branch ms = M (\s -> concatMap (`runM` s) ms)

tryM :: M a -> M (Either Err a)
tryM (M m) = M (map (either (\(e, s) -> Right (Left e, s)) (\(a, s) -> Right (Right a, s))) . m)

speculate :: M a -> M Bool
speculate (M m) = M (\s -> [Right (any isRight (m s), s)])

anyM :: (a -> M Bool) -> [a] -> M Bool
anyM p = \case
  [] -> pure False
  x : xs -> p x >>= \b -> if b then pure True else anyM p xs

allM :: (a -> M Bool) -> [a] -> M Bool
allM p = \case
  [] -> pure True
  x : xs -> p x >>= \b -> if b then allM p xs else pure False

findM :: (a -> M Bool) -> [a] -> M (Maybe a)
findM p = \case
  [] -> pure Nothing
  x : xs -> p x >>= \b -> if b then pure (Just x) else findM p xs

fresh :: M Term
fresh = do
  s <- get'
  put' s {counter = counter s + 1}
  pure (Meta (counter s))

bind :: Int -> Term -> M ()
bind m t = modify' (\s -> s {subst = Map.insert m t (subst s)})

unbind :: Int -> M ()
unbind m = modify' (\s -> s {subst = Map.delete m (subst s)})

share :: Term -> M Term
share = \case
  t@(App _ _) -> do
    m <- fresh
    case m of
      Meta k -> bind k t >> pure m
      _ -> pure t
  t -> pure t

lookupMeta :: Int -> M (Maybe Term)
lookupMeta m = gets' (Map.lookup m . subst)

nameMeta :: Name -> Term -> M ()
nameMeta n = \case
  Meta m -> modify' (\s -> s {names = Map.insert m n (names s)})
  _ -> pure ()

inst :: Int -> [Term] -> Term -> Term
inst depth fillers = \case
  Var d k
    | d == depth -> fillers !! k
    | d > depth -> Var (d - 1) k
    | otherwise -> Var d k
  App f a -> App (inst depth fillers f) (inst depth fillers a)
  Cons x rest -> Cons (inst depth fillers x) (inst depth fillers rest)
  Rule ns l r -> Rule ns (inst (depth + 1) fillers l) (inst (depth + 1) fillers r)
  t -> t

isHostName :: Name -> Bool
isHostName n = Map.member n valueHost

whnf :: Term -> M Term
whnf t = case t of
  Meta m ->
    lookupMeta m >>= \case
      Nothing -> pure t
      Just u -> do
        unbind m
        v <- whnf u
        lookupMeta m >>= \case
          Nothing -> when (v /= t) (bind m v) >> pure v
          Just w -> unify w v >> whnf t
  App f a -> do
    f' <- whnf f
    case f' of
      Sym n | isHostName n -> select t (hostCandidates n) a
      Sym n | n == answersName -> collect a
      Sym n | n == foreignName -> foreignOf t a
      Sym n | n == tieName -> knotOf a
      Rule {} -> throw (NotARule t)
      Cons {} -> do
        shared <- share a
        key <- gets' (\s -> App f' (resolveTerm s shared))
        reentered <- gets' (elem key . active)
        if reentered
          then fresh
          else do
            modify' (\s -> s {active = key : active s})
            result <- candidatesOf f' >>= \cs -> select t cs shared
            modify' (\s -> s {active = drop 1 (active s)})
            pure result
      Nil -> throw (NoRule t)
      _ -> pure (App f' a)
  _ -> pure t

knotOf :: Term -> M Term
knotOf f = do
  m <- fresh
  case m of
    Meta k -> bind k (App f m) >> whnf m
    _ -> throw (NoRule f)

foreignOf :: Term -> Term -> M Term
foreignOf node key =
  whnf key >>= \case
    Sym s | Just t <- Map.lookup (shown s) hostTable -> pure t
    _ -> throw (NoRule node)

collect :: Term -> M Term
collect question = do
  outer <- get'
  let results = runM (nf question) outer
  case [e | Left (e, _) <- results, fatal e] of
    e : _ -> throw e
    [] -> chain <$> mapM (\(t, inner) -> reimport (counter outer) inner (resolveTerm inner t)) [(t, s) | Right (t, s) <- results]

reimport :: Int -> S -> Term -> M Term
reimport since inner = fmap fst . go Map.empty
  where
    go memo = \case
      Meta m
        | m < since -> pure (Meta m, memo)
        | Just t <- Map.lookup m memo -> pure (t, memo)
        | otherwise -> do
            t <- fresh
            mapM_ (`nameMeta` t) (Map.lookup m (names inner))
            pure (t, Map.insert m t memo)
      App f a -> do
        (f', memo1) <- go memo f
        (a', memo2) <- go memo1 a
        pure (App f' a', memo2)
      Cons x rest -> do
        (x', memo1) <- go memo x
        (rest', memo2) <- go memo1 rest
        pure (Cons x' rest', memo2)
      Rule ns l r -> do
        (l', memo1) <- go memo l
        (r', memo2) <- go memo1 r
        pure (Rule ns l' r', memo2)
      t -> pure (t, memo)

resolveTerm :: S -> Term -> Term
resolveTerm s = go Set.empty
  where
    go seen = \case
      Meta m
        | Set.member m seen -> Meta m
        | Just u <- Map.lookup m (subst s) -> go (Set.insert m seen) u
        | otherwise -> Meta m
      App f a -> App (go seen f) (go seen a)
      Cons x rest -> Cons (go seen x) (go seen rest)
      Rule ns l r -> Rule ns (go seen l) (go seen r)
      t -> t

nf :: Term -> M Term
nf = go Set.empty
  where
    go seen t
      | Set.member t seen = pure t
      | otherwise =
          whnf t >>= \case
            App f a -> let seen' = Set.insert t seen in App <$> go seen' f <*> go seen' a
            Cons x rest -> let seen' = Set.insert t seen in Cons <$> go seen' x <*> go seen' rest
            v -> pure v

data Outcome = Body Term | Call ([Term] -> Maybe Term)

data Candidate = Candidate {left :: Term, outcome :: Outcome}

candidatesOf :: Term -> M [Candidate]
candidatesOf t = do
  t' <- whnf t
  case t' of
    Nil -> pure []
    Sym s | isHostName s -> pure (hostCandidates s)
    Cons x rest -> do
      here <- whnf x >>= instantiate
      (here :) <$> candidatesOf rest
    _ -> throw (NotARule t')

instantiate :: Term -> M Candidate
instantiate = \case
  Rule ns l r -> do
    ms <- mapM (const fresh) ns
    zipWithM_ nameMeta ns ms
    pure (Candidate (inst 0 ms l) (Body (inst 0 ms r)))
  t -> throw (NotARule t)

hostCandidates :: Name -> [Candidate]
hostCandidates n = case Map.lookup n valueHost of
  Just (HostFn shapes fn) -> [Candidate (shapeTerm sh) (Call fn) | sh <- shapes]
  Nothing -> []
  where
    shapeTerm = \case
      One ty -> literal ty
      Many tys -> chain (map literal tys)
    literal ty = App (constructorOf ty) (anyOf ty)
    constructorOf = \case
      Number -> numberSym
      String -> stringSym

data View = VHole | VSym Name | VAny HostType | VApp Term Term | VCons Term Term | VNil | VOther

view :: Term -> M View
view t =
  whnf t >>= \case
    Meta _ -> pure VHole
    Sym s -> pure (VSym s)
    v@(App f a) -> pure (maybe (VApp f a) VAny (anyType v))
    Cons x rest -> pure (VCons x rest)
    Nil -> pure VNil
    _ -> pure VOther

children :: View -> Maybe (Term, Term)
children = \case
  VApp f a -> Just (f, a)
  VCons x rest -> Just (x, rest)
  _ -> Nothing

data Dir = L | R deriving (Eq)

type Path = [Dir]

data Skeleton = SkSym Name | SkApp | SkCons | SkNil | SkAny deriving (Eq)

select :: Term -> [Candidate] -> Term -> M Term
select node cands arg = do
  lazy <- apart cands
  if lazy then go [] cands else enumerate cands
  where
    enumerate = branch . map attempt
    attempt c =
      tryM (unify (left c) arg) >>= \case
        Left (Mismatch _ _) -> throw (NoRule node)
        Left (Occurs _ _) -> throw (NoRule node)
        Left (Unbounded _) -> throw (Unbounded node)
        Left e -> throw e
        Right () -> fire c
    go forced cs = case cs of
      [] -> throw (NoRule node)
      [c] -> attempt c
      _ ->
        demanded forced cs >>= \case
          Nothing -> enumerate cs
          Just path ->
            forceAt path arg >>= \case
              Nothing -> throw (NoRule node)
              Just (at, Meta m) -> do
                sks <- skeletons at cs
                branch [bindSkeleton m sk >> go forced cs | sk <- sks]
              Just (_, v) -> filterM (compatibleAt path v) cs >>= go (path : forced)
    fire c = case outcome c of
      Body r -> whnf r
      Call fn -> do
        args <- hostArgs node arg
        maybe (throw (NoRule node)) whnf (fn args)
    bindSkeleton m = \case
      SkSym s -> bind m (Sym s)
      SkApp -> App <$> fresh <*> fresh >>= bind m
      SkCons -> Cons <$> fresh <*> fresh >>= bind m
      SkNil -> bind m Nil
      SkAny -> throw (Unbounded node)

pick :: Dir -> (Term, Term) -> Term
pick d (l, r) = if d == L then l else r

viewAt :: Path -> Term -> M (Maybe View)
viewAt [] t = Just <$> view t
viewAt (d : ds) t =
  view t >>= \v -> case children v of
    Just pair -> viewAt ds (pick d pair)
    Nothing -> pure Nothing

pathsOf :: Term -> M [Path]
pathsOf t =
  view t >>= \v -> case children v of
    Just (l, r) -> (\pl pr -> [] : map (L :) pl ++ map (R :) pr) <$> pathsOf l <*> pathsOf r
    Nothing -> pure [[]]

demanded :: [Path] -> [Candidate] -> M (Maybe Path)
demanded forced cs = case cs of
  [] -> pure Nothing
  c : _ -> pathsOf (left c) >>= findM ok
  where
    ok path
      | path `elem` forced = pure False
      | otherwise = allM (fmap rigid . viewAt path . left) cs
    rigid = \case
      Just VHole -> False
      Just _ -> True
      Nothing -> False

skeletons :: Path -> [Candidate] -> M [Skeleton]
skeletons path cs = nub . catMaybes <$> mapM (fmap (>>= skeletonOf) . viewAt path . left) cs
  where
    skeletonOf = \case
      VHole -> Nothing
      VSym s -> Just (SkSym s)
      VAny _ -> Just SkAny
      VApp _ _ -> Just SkApp
      VCons _ _ -> Just SkCons
      VNil -> Just SkNil
      VOther -> Nothing

compatibleAt :: Path -> Term -> Candidate -> M Bool
compatibleAt path v c = maybe False compatible <$> viewAt path (left c)
  where
    compatible = \case
      VSym s' | Sym s <- v -> s == s'
      VAny ty | Sym _ <- v -> hostTypeOf v == Just ty
      VApp _ _ | App _ _ <- v -> True
      VCons _ _ | Cons _ _ <- v -> True
      VNil | Nil <- v -> True
      _ -> False

forceAt :: Path -> Term -> M (Maybe (Path, Term))
forceAt = go []
  where
    go done [] t = whnf t >>= \v -> pure (Just (reverse done, v))
    go done (d : ds) t =
      whnf t >>= \case
        App f a -> go (d : done) ds (pick d (f, a))
        Cons x rest -> go (d : done) ds (pick d (x, rest))
        m@(Meta _) -> pure (Just (reverse done, m))
        _ -> pure Nothing

shallow :: Term -> M Term
shallow t = case t of
  Meta m ->
    lookupMeta m >>= \case
      Just u@(Meta _) -> shallow u
      _ -> pure t
  _ -> pure t

unbound :: Term -> M Bool
unbound = \case
  Meta m -> maybe True (const False) <$> lookupMeta m
  _ -> pure False

unify :: Term -> Term -> M ()
unify a b = do
  a' <- shallow a
  b' <- shallow b
  freeA <- unbound a'
  freeB <- unbound b'
  case (a', b') of
    (Meta m, Meta n) | m == n -> pure ()
    (Meta m, Meta n) | freeA && freeB -> if m < n then bind n a' else bind m b'
    _ | Just ty <- anyType a' -> whnf b' >>= anyAgainst ty
    _ | Just ty <- anyType b' -> whnf a' >>= anyAgainst ty
    (Meta m, u) | freeA -> bindOrUnbounded m u
    (u, Meta m) | freeB -> bindOrUnbounded m u
    _ -> do
      a'' <- whnf a'
      b'' <- whnf b'
      case (a'', b'') of
        (Meta _, _) -> unify a'' b''
        (_, Meta _) -> unify a'' b''
        (Sym x, Sym y) | x == y -> pure ()
        (App f x, App g y) -> unify f g >> unify x y
        (Cons x xs, Cons y ys) -> unify x y >> unify xs ys
        (Nil, Nil) -> pure ()
        (Rule {}, Rule {}) | a'' == b'' -> pure ()
        _ -> throw (Mismatch a'' b'')
  where
    anyAgainst ty v = case v of
      Sym _ | hostTypeOf v == Just ty -> pure ()
      Meta _ -> throw (Unbounded v)
      _ -> throw (Mismatch (anyOf ty) v)
    bindOrUnbounded m u
      | hasAny u = throw (Unbounded (Meta m))
      | otherwise = bindChecked m u
    hasAny t = case anyType t of
      Just _ -> True
      Nothing -> case t of
        App f x -> hasAny f || hasAny x
        Cons x rest -> hasAny x || hasAny rest
        _ -> False

bindChecked :: Int -> Term -> M ()
bindChecked m t = do
  bad <- occurs Set.empty t
  when bad (throw (Occurs m t))
  bind m t
  where
    occurs seen = \case
      Meta n
        | n == m -> pure True
        | Set.member n seen -> pure False
        | otherwise -> lookupMeta n >>= maybe (pure False) (occurs (Set.insert n seen))
      App f a -> (||) <$> occurs seen f <*> occurs seen a
      Cons x rest -> (||) <$> occurs seen x <*> occurs seen rest
      _ -> pure False

hostArgs :: Term -> Term -> M [Term]
hostArgs node arg = do
  v <- nf arg
  case inner v of
    Just x -> pure [x]
    Nothing -> case unchain v of
      (items, Nothing) | Just xs <- mapM inner items -> pure xs
      _ -> throw (NoRule node)
  where
    inner = \case
      App c x | c == numberSym || c == stringSym, hostTypeOf x /= Nothing -> Just x
      _ -> Nothing

apart :: [Candidate] -> M Bool
apart cands = not <$> anyM (\(p, q) -> overlap (left p) (left q)) (pairs cands)
  where
    pairs = \case
      [] -> []
      c : rest -> [(c, d) | d <- rest] ++ pairs rest

overlap :: Term -> Term -> M Bool
overlap p q = do
  (tp, sortsP) <- sorted p
  (tq, sortsQ) <- sorted q
  speculate $ do
    unify tp tq
    forM_ (sortsP ++ sortsQ) $ \(m, ty) ->
      whnf (Meta m) >>= \case
        Meta _ -> pure ()
        v@(Sym _) | hostTypeOf v == Just ty -> pure ()
        v -> throw (Mismatch (anyOf ty) v)
  where
    sorted t = case anyType t of
      Just ty -> do
        m <- fresh
        pure (m, [(k, ty) | Meta k <- [m]])
      Nothing -> case t of
        App f a -> do
          (tf, sf) <- sorted f
          (ta, sa) <- sorted a
          pure (App tf ta, sf ++ sa)
        Cons x rest -> do
          (tx, sx) <- sorted x
          (tr, sr) <- sorted rest
          pure (Cons tx tr, sx ++ sr)
        _ -> pure (t, [])

evaluate :: Program -> [Either (Err, S) (Term, S)]
evaluate p = runM (nf (middle p)) (initial p)

display :: S -> Term -> String
display s = pretty (namer s) . expandTerm s

namer :: S -> Int -> String
namer s m = maybe ("?" ++ show m) shown (Map.lookup m (names s))

expandTerm :: S -> Term -> Term
expandTerm s = expand Set.empty
  where
    expand seen = \case
      Meta m
        | Set.member m seen -> Meta m
        | otherwise -> case Map.lookup m (subst s) of
            Just u | not (Map.member m (names s)) || isData u -> expand (Set.insert m seen) u
            _ -> Meta m
      App f a -> App (expand seen f) (expand seen a)
      Cons x rest -> Cons (expand seen x) (expand seen rest)
      t -> t
    isData = \case
      Sym n -> not (isHostName n)
      Meta n -> maybe True isData (Map.lookup n (subst s))
      Rule {} -> False
      Nil -> True
      Cons x rest -> isData x && isData rest
      App f _ -> isData f
      _ -> False

describe :: S -> Err -> String
describe s = \case
  NoRule t -> "no rule: " ++ display s t
  Mismatch a b -> "mismatch: " ++ display s a ++ " against " ++ display s b
  Occurs m t -> "infinite: " ++ display s (Meta m) ++ " in " ++ display s t
  Unbounded t -> "ambiguous: " ++ display s t ++ " fits every host rule"
  NotARule t -> "only a chain can be applied: " ++ display s t
