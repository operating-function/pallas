{-
    This module is responsible for optimizing the let bindings found in
    PLAN laws.

    -   Read a law body, which includes a single LETREC at the top,
        followed by a tree of applications

    -   Split that LETREC into a series of LETs and smaller LETRECs.

    -   Move LETS and LETRECs as close to their usage-sites as possible.

    -   Renumber LETs and LETRECs, reusing slot numbers to use less
        stack space.

    -   Calculate the total stack size that's required.

    -   Convert from our internal representation to an unoptimized
        `Run` tree.
-}

module Fan.Eval.LetRec (Exp, loadLawBody, optimize, compile) where

import Fan.Types
import Fan.Util
import PlunderPrelude

import Control.Monad.State (State, runState, execState, modify')
import Data.Graph          (SCC(..), stronglyConnCompR)

import qualified Data.List as L


-- Types -----------------------------------------------------------------------

data Exp a
    = X_LET (a, Exp a) (Exp a)
    | X_REC [(a, Exp a)] (Exp a)
    | X_APP (Exp a) (Exp a)
    | X_REF Int
    | X_VAL Fan
  deriving (Functor)


-- Loading Expression from Law Bodies ------------------------------------------

loadLawBody :: Nat -> Fan -> ([Exp Void], Exp Void)
loadLawBody numArgs = goBinds []
  where
    goBinds binds fan =
        case kloList fan of
            [NAT 1, v, b] -> goBinds (v:binds) b
            _             -> let maxRef = numArgs + fromIntegral (length binds)
                             in (go maxRef <$> reverse binds, go maxRef fan)

    go :: Nat -> Fan -> Exp Void
    go maxRef fan =
        case kloList fan of
            [NAT n] | n<=maxRef -> X_REF (fromIntegral n)
            [NAT 0, f, x]       -> X_APP (go maxRef f) (go maxRef x)
            [NAT 2, x]          -> X_VAL x
            _                   -> X_VAL fan


-- Translating Expressions to `Run` Trees --------------------------------------

-- Translate to naive `Run` tree, and find the maximum reference
-- (in order to determine stack size).
compile :: Int -> Exp Int -> (Run, Int)
compile numArgs = flip runState 0 . go
  where
    goBind :: (Int, Exp Int) -> State Int (Int, Run)
    goBind (i, v) = do
        let ix = i - (numArgs+1)
        modify' (max ix)
        (ix,) <$> go v

    go :: Exp Int -> State Int Run
    go = \case
        X_VAL v             -> pure $ CNS $ v
        X_REF i | i>numArgs -> pure $ VAR $ fromIntegral (i - (numArgs+1))
        X_REF i             -> pure $ ARG $ fromIntegral i
        X_APP f x           -> KAL <$> (a2 <$> go f <*> go x)
        X_LET iv b          -> uncurry LET <$> goBind iv <*> go b
        X_REC vs b          -> do
            modify' \mr -> L.maximum (mr : map fst vs)
            LETREC <$> traverse goBind (smallArrayFromList vs) <*> go b


-- Splitting-Up LETREC Bindings ------------------------------------------------

splitBinds :: Int -> [Exp Void] -> Exp Void -> Exp Int
splitBinds numArgs allBinds =
    \body -> foldr step (absurd <$> body) components
  where
    cvt :: (Exp Void, Int, [Int]) -> (Int, Exp Int)
    cvt (v,i,_) = (i, absurd <$> v)

    step :: SCC (Exp Void, Int, [Int]) -> Exp Int -> Exp Int
    step (AcyclicSCC v) = X_LET (cvt v)
    step (CyclicSCC vs) = X_REC (cvt <$> vs)

    components :: [SCC (Exp Void, Int, [Int])]
    components =
        stronglyConnCompR $
        zip [numArgs+1..] allBinds <&> \(key, bind) ->
            let edges = toList $ execState (refs bind) mempty
            in (bind, key, edges)

    refs :: Exp Void -> State IntSet ()
    refs = \case
        X_REF r       -> modify' (insertSet r)
        X_VAL _       -> pure ()
        X_REC [] b    -> refs b
        X_REC (v:_) _ -> absurd (fst v)
        X_LET v _     -> absurd (fst v)
        X_APP f x     -> refs f >> refs x


-- Optimizing Expressions ------------------------------------------------------

optimize :: Int -> ([Exp Void], Exp Void) -> Exp Int
optimize numArgs (topBinds, topBody) =
    ( renumberBinds numArgs
    . lowerBinds
    . splitBinds numArgs topBinds
    ) topBody

references :: (Int -> Bool) -> Exp Int -> Bool
references chk = \case
    X_REF k       -> chk k
    X_VAL _       -> False
    X_LET (_,v) b -> references chk v || references chk b
    X_APP f x     -> references chk f || references chk x
    X_REC vs b    -> any (references chk . snd) vs || references chk b

-- Move lets as close as possible to their usage-sites.
--
-- TODO: current implementation of lowerBinder is very expensive.  O(n^2)?
lowerBinds :: Exp Int -> Exp Int
lowerBinds = \case
    X_VAL v    -> X_VAL v
    X_REF r    -> X_REF r
    X_APP f x  -> X_APP (lowerBinds f) (lowerBinds x)
    X_LET v b  -> lowerLet v (lowerBinds b)
    X_REC vs b -> lowerRec (setFromList (fst <$> vs)) vs (lowerBinds b)
  where
    lowerRec :: Set Int -> [(Int, Exp Int)] -> Exp Int -> Exp Int
    lowerRec bet binds = lowerBinder (`member` bet) (X_REC binds)

    lowerLet :: (Int, Exp Int) -> Exp Int -> Exp Int
    lowerLet bind@(i,_) = lowerBinder (==i) (X_LET bind)

    lowerBinder :: (Int -> Bool) -> (Exp Int -> Exp Int) -> Exp Int -> Exp Int
    lowerBinder chk wrap scrut =
      let recur = lowerBinder chk wrap in
      case scrut of
        X_VAL k   -> X_VAL k
        X_REF v   -> if chk v then wrap (X_REF v) else X_REF v
        X_APP f x ->
            case (references chk f, references chk x) of
                (False, False) -> X_APP f x
                (True, True)   -> wrap (X_APP f x)
                (True, False)  -> X_APP (recur f) x
                (False, True)  -> X_APP f (recur x)
        X_LET (j,v) b ->
            case (references chk v, references chk b) of
                (False, False) -> X_LET (j,v) b
                (True, True)   -> wrap (X_LET (j,v) b)
                (True, False)  -> X_LET (j,recur v) b
                (False, True)  -> X_LET (j,v) (recur b)
        X_REC vs b -> do
            let bindRefs = references chk . snd <$> vs
            case (sum (bool 0 1 <$> bindRefs)::Int, references chk b) of
                (0, False) -> X_REC vs b
                (0, True)  -> X_REC vs (recur b)
                (_, True)  -> wrap (X_REC vs b)
                (1, _)     -> X_REC (place bindRefs vs) b
                (_, _)     -> wrap (X_REC vs b)
              where
                place :: [Bool] -> [(Int, Exp Int)] -> [(Int, Exp Int)]
                place (False:flags) (kb:more)     = kb : place flags more
                place (True:_)      ((k,bx):more) = (k, recur bx) : more
                place _             more          = more

{-
    This just reassigns binding numbers in stack order, maintaining a
    "scope" (a mapping from old bind numbers to new bind numbers).
-}
renumberBinds :: Int -> Exp Int -> Exp Int
renumberBinds numArgs =
    go (numArgs+1) mempty
  where
    go :: Int -> IntMap Int -> Exp Int -> Exp Int
    go n t = \case
        X_VAL k       -> X_VAL k
        X_REF i       -> X_REF $ if i <= numArgs then i else
                                 fromMaybe (error "impossible!") (lookup i t)
        X_APP f x     -> X_APP (go n t f) (go n t x)
        X_LET (i,v) b -> X_LET (n, go n t v) (go (n+1) (insertMap i n t) b)
        X_REC vs b    -> X_REC (zip newIxs (go n' t' . snd <$> vs)) (go n' t' b)
          where newIxs = fst <$> zip [n..] vs
                t'     = foldr (uncurry insertMap) t $ zip (fst <$> vs) newIxs
                n'     = n + length vs
