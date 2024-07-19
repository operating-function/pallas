-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.ReadT
    ( Result(..)
    , ResultT(..)
    , resultEither
    , commit
    , ReadT(..)
    , Reading
    , doRead
    , readThis
    , matchLeaf, leaf, matchName, matchCord, matchLineStr
    , matchLeafCont, matchNameCord, matchNameText
    , formMatch, formMatchNoCont, formMatchCont, formMatchEither
    , subTree
    , rune
    , readRex
    , readNode
    , readExtra
    , readResult
    , form0, form1, form2, form3, form4, formN
    , form1N, formN1, formN1c, form1N1c
    , form0C, form1C, form2C, form3C, form4C, formNC, form1NC
    , form0c, form1c, form2c, form3c, form4c, formNc, form1Nc
    , form0e, form1e, form2e, form3e, form4e, formNe, form1Ne
    , slip1, slip2, slip3, slipN, slip1N
    , slip_1_2, slip_N_2
    , form2N
    )
where

import PlunderPrelude hiding (bracket, exp, many, some, try)

import Control.Monad.Trans.Class (MonadTrans(..))
import Rex.Types                 (GRex(..), Leaf, TextShape(..))

import qualified Data.Text as T

-- Result ----------------------------------------------------------------------

data Result z a
  = Success a
  | Failure (Maybe (GRex z), Text)
  | Expected [(GRex z, Text)]
 deriving (Eq, Ord, Show, Functor)

resultEither :: Result z a -> Either (Maybe(GRex z), Text) a
resultEither = \case
  Failure (x,t)        -> Left (x,t)
  Success a            -> Right a
  Expected []          -> Left (Nothing, "empty parse")
  Expected [(e,t)]     -> Left (Just e, "Expected:\n\n" <> indent t)
  Expected ((e,t):ms)  -> Left (Just e, (<>) "Expected one of:\n\n"
                                             (indent $ intercalate "\n" ts))
                           where ts = t : map snd ms
 where
  indent :: Text -> Text
  indent = unlines . map ("    " <>) . lines

commit :: Result z a -> Result z a
commit = either Failure Success . resultEither

instance Semigroup (Result z a) where
    Success s   <> _          = Success s
    Failure f   <> _          = Failure f
    Expected _  <> Success s  = Success s
    Expected _  <> Failure f  = Failure f
    Expected m  <> Expected n = Expected (m <> n)

instance Monoid (Result z a) where
    mempty = Expected []

instance Applicative (Result z) where
    pure = Success
    (<*>) = ap

instance Monad (Result z) where
    Success x  >>= f = f x
    Failure e  >>= _ = Failure e
    Expected e >>= _ = Expected e

instance Monad m => MonadError Text (ReadT z m) where
    catchError = error "whatever"

    throwError = \e -> READT (\rex -> REST
                                    $ pure
                                    $ Failure (Just rex, (T.stripEnd e <> "\n"))
                             )



-- Reading ---------------------------------------------------------------------

newtype ResultT z m a = REST { runResultT :: m (Result z a) }
  deriving (Functor)

newtype ReadT z m a = READT { runReadT :: GRex z -> ResultT z m a }
  deriving (Functor)

type Reading z = ReadT z Identity

doRead :: Reading z a -> GRex z -> Result z a
doRead (READT act) = runIdentity . runResultT . act

readThis :: ReadT z m a -> GRex z -> ReadT z m a
readThis reader rex = READT $ const $ runReadT reader rex

instance MonadIO m => MonadIO (ReadT z m) where
    liftIO a = READT (\_ -> REST $ fmap Success $ liftIO a)

instance Monad m => Alternative (ResultT z m) where
    empty = REST $ pure $ Expected []

    REST a <|> REST b =
        REST do
            a >>= \case
              Success s  -> pure $ Success s
              Failure f  -> pure $ Failure f
              Expected m -> do
                b >>= \case
                  Success s  -> pure $ Success s
                  Failure f  -> pure $ Failure f
                  Expected n -> pure $ Expected (m <> n)

instance Monad m => Alternative (ReadT z m) where
    empty = READT $ const empty

    READT a <|> READT b = READT (\x -> a x <|> b x)

-- instance Monoid (ReadT z m a) where
    -- mempty = READT mempty

instance Monad m => Applicative (ResultT z m) where
    pure = REST . pure . Success
    (<*>) = ap

instance Monad m => Applicative (ReadT z m) where
    pure = READT . const . pure
    (<*>) = ap

instance MonadTrans (ReadT z) where
    lift = READT . const . REST . fmap Success

instance Monad m => Monad (ResultT z m) where
    REST a >>= f = REST do
        a >>= \case
            Failure  e -> pure $ Failure e
            Expected x -> pure $ Expected x
            Success  x -> runResultT $ f x

instance Monad m => Monad (ReadT z m) where
    r >>= f =
        READT \x ->
            REST $ runResultT (runReadT r x) >>= \case
                Failure e  -> pure $ Failure e
                Expected m -> pure $ Expected m
                Success v  -> do
                    fmap resultEither (runResultT (runReadT (f v) x)) >>= \case
                        Left er -> pure $ Failure er
                        Right a -> pure $ Success a


-- Building Blocks -------------------------------------------------------------

readResult :: Monad m => Result z a -> ReadT z m a
readResult = READT . const . REST . pure

readNode :: Monad m => ReadT z m (Text, [GRex z], Maybe (GRex z))
readNode = READT $ \case
    N _ r xs mK -> resty $ Success (r, xs, mK)
    expr        -> resty $ Expected [(expr, "node")]

readExtra :: ∀m z. Monad m => ReadT z m z
readExtra = READT \case
    C c -> REST $ pure $ Success c
    rex -> REST $ pure $ Expected [(rex, "Embeded Value")]

readRex :: Monad m => ReadT z m (GRex z)
readRex = READT pure

resty :: Monad m => Result z a -> ResultT z m a
resty = REST . pure

rune :: Monad m => Text -> ReadT z m ()
rune r = READT go
 where
  go (N _ run _ _) | run==r = resty $ Success ()
  go expr                   = resty $ Expected [(expr, r)]

matchLeaf :: Monad m => Text -> (Leaf -> Maybe a) -> ReadT z m a
matchLeaf expect f = READT \case
  x@(T s t Nothing) -> resty $ maybe (Expected [(x, expect)]) pure (f (s,t))
  x                 -> resty $ Expected [(x, expect)]

matchLeafCont :: Monad m => Text -> (Leaf -> GRex z -> Maybe a) -> ReadT z m a
matchLeafCont expect f = READT \case
  x@(T s t (Just k)) -> resty $ maybe (Expected [(x, expect)]) pure (f (s,t) k)
  x                  -> resty $ Expected [(x, expect)]

leaf :: Monad m => ReadT z m Leaf
leaf = matchLeaf "leaf" Just

matchName :: Monad m => Text -> (Text -> Maybe a) -> ReadT z m a
matchName expect f = matchLeaf expect \case
  (WORD, n) -> f n
  _              -> Nothing

matchNameCord :: Monad m => Text -> (Text -> Text -> Maybe a) -> ReadT z m a
matchNameCord expect f =
    matchLeafCont expect go
  where
    go (WORD, n) (T TEXT t Nothing) = f n t
    go _         _                  = Nothing

matchNameText :: Monad m => Text -> (Text -> Text -> Maybe a) -> ReadT z m a
matchNameText expect f =
    matchLeafCont expect go
  where
    go (WORD, n) (T TEXT t Nothing) = f n t
    go (WORD, n) (T LINE t Nothing) = f n t
    go _         _                  = Nothing

matchCord :: Monad m => Text -> (Text -> Maybe a) -> ReadT z m a
matchCord expect f = matchLeaf expect \case
  (TEXT, t) -> f t
  _         -> Nothing

matchLineStr :: Monad m => Text -> (Text -> Maybe a) -> ReadT z m a
matchLineStr expect f = matchLeaf expect \case
  (LINE, l) -> f l
  _         -> Nothing

formMatch
    :: Monad m
    => (GRex z -> ([GRex z], Maybe (GRex z)) -> ResultT z m c)
    -> ReadT z m c
formMatch f = READT \case
  x@(N _ _ ps mK) -> f x (ps, mK)
  x               -> resty $ Expected [(x, "expression")]

formMatchNoCont
    :: Monad m
    => (GRex z -> [GRex z] -> ResultT z m c)
    -> ReadT z m c
formMatchNoCont f = READT \case
  x@(N _ _ ps Nothing) -> f x ps
  x@(N _ _ _  _)       -> resty $ Expected [(x, "no heir")]
  x                    -> resty $ Expected [(x, "runic")]

formMatchCont
    :: Monad m
    => Text
    -> (GRex z -> ([GRex z], GRex z) -> ResultT z m c)
    -> ReadT z m c
formMatchCont t f =
  formMatch \x (ps, mK) ->
    case mK of
      Nothing -> resty $ Expected [(x, t <> " w/ heir")]
      Just k  -> f x (ps, k)

formMatchEither
    :: Monad m
    => (GRex z -> [GRex z] -> ResultT z m c) -> ReadT z m c
formMatchEither f =
  formMatch \x (ps, mK) -> f x (ps <> toList mK)


-- Expressions Without Heir ----------------------------------------------------

form0 :: Monad m => ReadT z m ()
form0 = formMatchNoCont \x ps ->
  case ps of
    [] -> pure ()
    _  -> resty $ Expected [(x, "no kids")]

subTree :: Monad m => ReadT z m a -> GRex z -> ResultT z m a
subTree f x = REST $ (commit <$> runResultT (runReadT f x))

form1 :: Monad m => ReadT z m a -> ReadT z m a
form1 f = formMatchNoCont \x ps ->
  case ps of
    [p] -> subTree f p
    _   -> resty $ Expected [(x, "one kid")]

form2 :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m (a, b)
form2 f g = formMatchNoCont \x ps ->
  case ps of
    [p,q] -> (,) <$> subTree f p <*> subTree g q
    _     -> resty $ Expected [(x, "two kids")]

form3 :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a, b, c)
form3 f g h = formMatchNoCont \x ps ->
  case ps of
  [p, q, r] -> (,,) <$> subTree f p <*> subTree g q <*> subTree h r
  _         -> resty $ Expected [(x, "three kids")]

form4
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m d
    -> ReadT z m (a, b, c, d)
form4 f g h i = formMatchNoCont \x ps ->
  case ps of
  [p, q, r, s] -> (,,,) <$> subTree f p
                        <*> subTree g q
                        <*> subTree h r
                        <*> subTree i s
  _            -> resty $ Expected [(x, "four kids")]

formN :: Monad m => ReadT z m a -> ReadT z m [a]
formN f = formMatchNoCont \_ ps -> traverse (subTree f) ps

form1N :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m (a, [b])
form1N p q =
  formMatchNoCont $ \x ps ->
    case ps of
      []   -> resty $ Expected [(x, "At least one kid")]
      y:ys -> (,) <$> subTree p y <*> traverse (subTree q) ys

form2N :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a, b, [c])
form2N p q r =
  formMatchNoCont $ \x ps ->
    case ps of
      []     -> resty $ Expected [(x, "At least two kids")]
      [_]    -> resty $ Expected [(x, "At least two kids")]
      y:z:ys -> (,,) <$> subTree p y <*> subTree q z <*> traverse (subTree r) ys


formN1 :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m ([a], b)
formN1 p q =
  formMatchNoCont $ \x ps ->
    case reverse ps of
      []   -> resty $ Expected [(x, "At least one kid")]
      y:ys -> (,) <$> traverse (subTree p) (reverse ys) <*> subTree q y


-- Expressions With Heir -------------------------------------------------------

form0C :: Monad m => ReadT z m a -> ReadT z m a
form0C f = formMatchCont "no kids" \x ps ->
  case ps of
    ([], k) -> subTree f k
    _       -> resty $ Expected [(x, "no kids")]

form1C :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m (a,b)
form1C f c = formMatchCont "one kid" \x ps ->
  case ps of
    ([p], k) -> (,) <$> subTree f p <*> subTree c k
    _        -> resty $ Expected [(x, "one kid")]

form2C :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a,b,c)
form2C f g c = formMatchCont "two kids" \x ps ->
  case ps of
    ([p,q], k) -> (,,) <$> subTree f p <*> subTree g q <*> subTree c k
    _          -> resty $ Expected [(x, "two kids")]

form3C
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m d -> ReadT z m (a,b,c,d)
form3C f g h c = formMatchCont "three kids" \x ps ->
  case ps of
    ([p,q,r],k) -> (,,,) <$> subTree f p
                         <*> subTree g q
                         <*> subTree h r
                         <*> subTree c k
    _           -> resty $ Expected [(x, "three kids")]

form4C
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m d -> ReadT z m e
    -> ReadT z m (a, b, c, d, e)
form4C f g h i c = formMatchCont "four kids" \x ps ->
  case ps of
    ([p, q, r, s], k) -> (,,,,) <$> subTree f p
                                <*> subTree g q
                                <*> subTree h r
                                <*> subTree i s
                                <*> subTree c k
    _                 -> resty $ Expected [(x, "four kids")]

formNC
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m ([a], b)
formNC f c = formMatchCont "kids" \_ (ps,k) -> (,) <$> traverse (subTree f) ps
                                                     <*> subTree c k


-- Expressions With Optional Heir ----------------------------------------------

form0c :: Monad m => ReadT z m ()
form0c = formMatchEither \x ps ->
  case ps of
    [] -> pure ()
    _  -> resty $ Expected [(x, "no kids")]

form1c :: Monad m => ReadT z m a -> ReadT z m a
form1c f = formMatchEither \x ps ->
  case ps of
    [p] -> subTree f p
    _   -> resty $ Expected [(x, "one kids")]

form2c :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m (a, b)
form2c f g = formMatchEither \x ps ->
  case ps of
    [p,q] -> (,) <$> subTree f p <*> subTree g q
    _     -> resty $ Expected [(x, "two kids")]

form3c :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a, b, c)
form3c f g h = formMatchEither \x ps ->
  case ps of
    [p, q, r] -> (,,) <$> subTree f p <*> subTree g q <*> subTree h r
    _         -> resty $ Expected [(x, "three kids")]

form4c
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m d
    -> ReadT z m (a, b, c, d)
form4c f g h i = formMatchEither \x ps ->
  case ps of
    [p, q, r, s] -> (,,,) <$> subTree f p
                          <*> subTree g q
                          <*> subTree h r
                          <*> subTree i s
    _            -> resty $ Expected [(x, "four kids")]

formNc :: Monad m => ReadT z m a -> ReadT z m [a]
formNc f = formMatchEither \_ ps -> traverse (subTree f) ps

form1Nc :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m (a, [b])
form1Nc p q =
  formMatchEither $ \x ps ->
    case ps of
      []   -> resty $ Expected [(x, "At least one kid")]
      y:ys -> (,) <$> subTree p y <*> traverse (subTree q) ys

form1NC :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a, [b], c)
form1NC p q r =
  formMatchCont "one kids" $ \x (ps,k) ->
    case ps of
      []   -> resty $ Expected [(x, "At least one kid")]
      y:ys -> (,,) <$> subTree p y <*> traverse (subTree q) ys <*> subTree r k

formN1c :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m ([a], b)
formN1c p q =
  formMatchEither $ \x ps ->
    case reverse ps of
      []   -> resty $ Expected [(x, "At least one kid")]
      y:ys -> (,) <$> traverse (subTree p) (reverse ys) <*> subTree q y

form1N1c
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c
    -> ReadT z m (a, [b], c)
form1N1c p q r =
  formMatchEither $ \x ps ->
    case ps of
      []   -> resty $ Expected [(x, "At least two kids")]
      y:ys -> case reverse ys of
                [] -> resty $ Expected [(x, "At least two kids")]
                z:zs -> (,,) <$> subTree p y
                             <*> traverse (subTree q) (reverse zs)
                             <*> subTree r z


-- Expressions With Explicit Optional Heir -------------------------------------

form0e :: Monad m => ReadT z m a -> ReadT z m (Maybe a)
form0e f = formMatch \x (ps, k) ->
  case ps of
    [] -> traverse (subTree f) k
    _  -> resty $ Expected [(x, "no kids")]

form1e :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m (a, Maybe b)
form1e f c = formMatch \x (ps, k) ->
  case ps of
    [p] -> (,) <$> subTree f p <*> traverse (subTree c) k
    _   -> resty $ Expected [(x, "one child")]

form2e
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a, b, Maybe c)
form2e f g c = formMatch \x (ps, k) ->
  case ps of
    [p,q] -> (,,) <$> subTree f p <*> subTree g q <*> traverse (subTree c) k
    _     -> resty $ Expected [(x, "two kids")]

form3e
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m d
    -> ReadT z m (a,b,c, Maybe d)
form3e f g h c = formMatch \x (ps, k) ->
  case ps of
    [p,q,r] -> (,,,) <$> subTree f p
                     <*> subTree g q
                     <*> subTree h r
                     <*> traverse (subTree c) k
    _       -> resty $ Expected [(x, "three kids")]

form4e
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m d -> ReadT z m e
    -> ReadT z m (a, b, c, d, Maybe e)
form4e f g h i c = formMatch \x (ps, k) ->
  case ps of
    [p, q, r, s] -> (,,,,) <$> subTree f p
                           <*> subTree g q
                           <*> subTree h r
                           <*> subTree i s
                           <*> traverse (subTree c) k
    _            -> resty $ Expected [(x, "four kids")]

formNe :: Monad m => ReadT z m a -> ReadT z m b -> ReadT z m ([a], Maybe b)
formNe f c = formMatch \_ (ps,k) -> (,) <$> traverse (subTree f) ps
                                        <*> traverse (subTree c) k

form1Ne
    :: Monad m
    => ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m (a, [b], Maybe c)
form1Ne p q c =
  formMatch \x (ps, k) ->
    case ps of
      []   -> resty $ Expected [(x, "At least one kid")]
      y:ys -> (,,) <$> subTree p y
                   <*> traverse (subTree q) ys
                   <*> traverse (subTree c) k

-- Slip Runes ------------------------------------------------------------------

slip1 :: Monad m => Text -> ReadT z m a -> ReadT z m [a]
slip1 n p = loop
 where
  next = rune n >> loop
  loop = asum [ singleton <$> form1 p
              , uncurry (:) <$> form1C p next
              ]

slip2 :: Monad m => Text -> ReadT z m a -> ReadT z m b -> ReadT z m [(a, b)]
slip2 n p q = loop
 where
  next = rune n >> loop
  loop = asum [ form2C p q next <&> \(a, b, c) -> (a,b):c
              , form2c p q      <&> \(a, b) -> [(a, b)]
              ]

slip3
    :: Monad m
    => Text -> ReadT z m a -> ReadT z m b -> ReadT z m c -> ReadT z m [(a, b, c)]
slip3 n p q r = loop
 where
  next = rune n >> loop
  loop = asum [ form3C p q r next <&> \(a, b, c, k) -> (a,b,c):k
              , form3c p q r      <&> \x -> [x]
              ]

slipN :: Monad m => Text -> ReadT z m a -> ReadT z m [[a]]
slipN n p = loop
 where
  next = rune n >> loop
  loop = asum [ formNC p next <&> \(xs,c) -> xs:c
              , formN p       <&> \xs     -> [xs]
              ]

-- TODO If no elements given, don't give error saying that not giving
-- a heir would be okay.
slip1N :: Monad m => Text -> ReadT z m a -> ReadT z m b -> ReadT z m [(a,[b])]
slip1N n p q = loop
 where
  next = rune n >> loop
  loop = asum [ form1NC p q next <&> \(x,ys,c) -> ((x,ys):c)
              , form1N p q       <&> \(x,ys)   -> [(x,ys)]
              ]


{-
    Unlike the other slip forms, this one cannot be combined with other
    forms under the same starting rune.  We need to examine the starting
    rune in order to determine which of the two forms it is.
-}
slip_1_2
    :: Monad m
    => (Text, ReadT z m a)
    -> (Text, ReadT z m b, ReadT z m c)
    -> ReadT z m [Either a (b, c)]
slip_1_2 (ra,a) (rp,p,q) = loop
 where
  loop = asum [ rune ra >> formA
              , rune rp >> formP
              ]
  formA = asum [ form1 a       <&> \x     -> [Left x]
               , form1C a loop <&> \(x,c) -> (Left x):c
               ]

  formP = asum [ form2 p q       <&> \(x,y)   -> [Right (x,y)]
               , form2C p q loop <&> \(x,y,c) -> (Right (x,y)):c
               ]

slip_N_2
    :: Monad m
    => (Text, ReadT z m a)
    -> (Text, ReadT z m b, ReadT z m c)
    -> ReadT z m [Either [a] (b, c)]
slip_N_2 (ra,a) (rp,p,q) = loop
 where
  loop = asum [ rune ra >> formA
              , rune rp >> formP
              ]
  formA = asum [ formN a       <&> \xs     -> [Left xs]
               , formNC a loop <&> \(xs,c) -> (Left xs):c
               ]

  formP = asum [ form2 p q       <&> \(x,y)   -> [Right (x,y)]
               , form2C p q loop <&> \(x,y,c) -> (Right (x,y)):c
               ]
