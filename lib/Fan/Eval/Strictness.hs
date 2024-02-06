-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-
    This module contains the `optimizeSpine` transformation on Run,
    which takes advantages of cases where we know that something will
    be evaluated strictly.

    For example, if I have this input:

        & c
        (if c (fib c) (ack c c))

     Because I know that this `if` will be evaluated strictly (because
     it's "on the spine" of the law), then I can convert that to
     control flow.

         # if c
         # then (fib c)
         # else (ack c c)

     And that control flow can be executed *during law substiution*, which
     avoids the need to allocate thunks for both branches of the `if`.

     We also take this a step further, and, if we find an opportunity
     to apply this optimization to a significant subgraph that *is not*
     on the spine, then we "shatter the spine" by splitting out that
     subgraph into a separate "pseudo-law" with it's own spine.

     For example:

         & c
         | add 3
         | if c (fib c) (ack c c)

     Becomes:

         & c
         | add 3
             ^ c
             & c
             # if c
             # then (fib c)
             # else (ack c c)

     Because this transformation happens *within a thunk*, the pseudo
     law isn't observable.  This has a bunch of overhead (another stack
     frame, copy free variables), but it also means that we don't have
     to allocate that whole sub-graph until it's thunk is actually forced.
-}

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}

module Fan.Eval.Strictness (optimizeSpine) where

import Data.Sorted
import Fan.RunHashes
import Fan.Types
import PlunderPrelude  hiding (hash)

import Control.Monad.Trans.State (State, execState, modify', runState)
import GHC.Word                  (Word(..))


--------------------------------------------------------------------------------

isWord :: Fan -> Bool
isWord (NAT (NatS# _)) = True
isWord _               = False

{-
    {mkJumpTable} is a small utility for constructing a `JMP_WORD` node.
-}
mkJumpTable :: Run -> Run -> Tab Fan Run -> Run
mkJumpTable key fal vals =
    JMP_WORD key fal jmpKeys jmpVals
  where
    jmpKeys = fromList (fst <$> pairs)
    jmpVals = smallArrayFromList (snd <$> pairs)
    pairs   = wordify (tabToAscPairsList vals)

    wordify :: [(Fan, Run)] -> [(Word, Run)]
    wordify []                        = []
    wordify ((NAT (NatS# k), v) : vs) = (W# k, v) : wordify vs
    wordify ((_,             _) : _ ) = error "no good"

optimizeSpine :: Run -> Run
optimizeSpine = go
  where
    -- We are on the law spine, so everything we see is demanded, safe
    -- to replace calls to functions like `if` with control flow like `IF_`
    go = \case
        exe@(EXE _ _ (PIN p) r) ->
            let haz = p.hash in
            if | haz == ifHash        -> IF_ (go(r.!0)) (go(r.!1)) (go(r.!2))
               | haz == ifzHash       -> IFZ (go(r.!0)) (go(r.!1)) (go(r.!2))
               | haz == switchHash    -> goSwitch exe r
               | haz == tabSwitchHash -> goTabSwitch exe r
               | haz == seqHash       -> SEQ (go(r.!0)) (go(r.!1))
               | haz == traceHash     -> TRK (go(r.!0)) (go(r.!1))
               | otherwise            -> exe
        LET i v b   -> LET i (goLazy v) (go b)
        LETREC vs b -> LETREC (fmap goLazy <$> vs) (go b)
        exe         -> goLazy exe

    goSwitch exe r =
        case r.!2 of
            MK_ROW vrun ->
                SWI (go $ r.!0)
                    (go $ r.!1)
                    (go <$> (smallArrayFromList $ toList vrun))
            _ -> exe

    goTabSwitch exe r = case r.!2 of
        MK_TAB vs ->
            if all isWord (tabKeysArray vs) && (length vs < 100) then
                mkJumpTable (go (r.!0))
                            (go (r.!1))
                            (go <$> vs)
            else
                JMP (go (r.!0))
                    (go (r.!1))
                    (go <$> vs)
        _ -> exe

    -- We are no longer on the spine, the expressions that we are looking
    -- at may not be demanded.
    goLazy run =
        case run of
            -- These branches are only introduced by this pass, if we
            -- see them in the input, something has run afowl.
            IF_{}      -> error "goLazy: impossible"
            IFZ{}      -> error "goLazy: impossible"
            SWI{}      -> error "goLazy: impossible"
            JMP{}      -> error "goLazy: impossible"
            JMP_WORD{} -> error "goLazy: impossible"
            SEQ{}      -> error "goLazy: impossible"
            LAZ{}      -> error "goLazy: impossible"
            TRK{}      -> error "goLazy: impossible"

            CNS{} -> run
            ARG{} -> run
            VAR{} -> run

            OP2 f o x y -> OP2 f o (goLazy x) (goLazy y)

            REC vs      -> REC (goLazy <$> vs)
            KAL vs      -> KAL (goLazy <$> vs)
            PAR r vs    -> PAR r (goLazy <$> vs)
            LET i v b   -> LET i (goLazy v) (goLazy b)
            LETREC vs b -> LETREC (fmap goLazy <$> vs) (goLazy b)
            MK_ROW xs   -> MK_ROW (goLazy <$> xs)
            MK_TAB vs   -> MK_TAB (goLazy <$> vs)

            -- If we see something that we want to turn into control
            -- flow, we can't do that safely on the main spine, because it
            -- may not be demanded.  However, if we shatter it into a
            -- broken-out sub-spine, we can optimize that and then run
            -- it with `LAZ`.
            EXE x s (PIN p) r ->
                let haz = p.hash in
                if | haz == ifHash        -> shatter run x s p r
                   | haz == switchHash    -> shatter run x s p r
                   | haz == tabSwitchHash -> shatter run x s p r
                   | haz == seqHash       -> shatter run x s p r
                   | haz == traceHash     -> shatter run x s p r
                   | otherwise            -> EXE x s (PIN p) (goLazy <$> r)

            EXE x s f r -> EXE x s f (goLazy <$> r)

    shatter :: Run -> (SmallArray Fan -> Fan) -> Int -> Pin -> SmallArray Run -> Run
    shatter run x s p r =
        if runSize (EXE x s (PIN p) r) < 16
        then EXE x s (PIN p) (goLazy <$> r)
        else LAZ pro{prgrm=go pro.prgrm} (goLazy <$> arg)
      where
         (pro, arg) = shatterSpine run

{-
    This just counts the number of nodes in the tree.  Used for very
    rough heuristics.
-}
runSize :: Run -> Int
runSize = w
  where
    w = \case
        CNS{}            -> 1
        ARG{}            -> 1
        VAR{}            -> 1
        LET _ x y        -> 1 + w x + w y
        LETREC vx y      -> 1 + sum (w . snd <$> vx) + w y
        IF_ c t e        -> 1 + w c + w t + w e
        IFZ c t e        -> 1 + w c + w t + w e
        EXE _ _ _ x      -> 1 + sum (w <$> x)
        OP2 _ _ x y      -> 1 + w x + w y
        SWI c f v        -> 1 + w c + w f + sum (w <$> v)
        JMP c f vs       -> 1 + w c + w f + sum (w <$> vs)
        JMP_WORD c f _ v -> 1 + w c + w f + sum (w <$> v)
        SEQ x y          -> 1 + w x + w y
        REC xs           -> 1 + sum (w <$> xs)
        KAL xs           -> 1 + sum (w <$> xs)
        PAR _ xs         -> 1 + sum (w <$> xs)
        TRK x y          -> 1 + w x + w y
        MK_ROW xs        -> 1 + sum (w <$> xs)
        MK_TAB xs        -> 1 + sum (w <$> xs)
        LAZ _ arg        -> 1 + sum (w <$> arg)

type Refr = Either Int Int

freeVarsForRun :: Run -> [Refr]
freeVarsForRun =
    \top -> toList $ execState (go mempty top) mempty
  where
    go :: Set Refr -> Run -> State (Set Refr) ()
    go z = \case
        ARG 0            -> pure () -- self reference
        ARG v            -> addRef z (Left v)
        VAR v            -> addRef z (Right v)
        CNS{}            -> pure ()
        SEQ a b          -> go z a >> go z b
        TRK a b          -> go z a >> go z b
        KAL xs           -> traverse_ (go z) xs
        PAR _ xs         -> traverse_ (go z) xs
        REC xs           -> traverse_ (go z) xs
        MK_ROW xs        -> traverse_ (go z) xs
        MK_TAB xs        -> traverse_ (go z) xs
        LAZ _prg xs      -> traverse_ (go z) xs
        SWI c f v        -> go z c >> go z f >> traverse_ (go z) v
        JMP c f xs       -> go z c >> go z f >> traverse_ (go z) xs
        JMP_WORD c f _ v -> go z c >> go z f >> traverse_ (go z) v
        EXE _ _ _ r      -> traverse_ (go z) r
        OP2 _ _ a b      -> go z a >> go z b
        IF_ i t e        -> go z i >> go z t >> go z e
        IFZ i t e        -> go z i >> go z t >> go z e
        LET i v b        -> go z v >> go (insertSet (Right i) z) b
        LETREC vs b      -> do let z' = foldr insertSet z (Right . fst <$> vs)
                               traverse_ (go z' . snd) vs
                               go z' b

    addRef :: Set Refr -> Refr -> State (Set Refr) ()
    addRef z x = unless (x `member` z) (modify' $ insertSet x)

{-
    {shatterSpine} turns a sub-expression into, essentially, it's own law.
    This allows us perform spine optimizations (like treating
    if/switch/etc as control flow) on lazy sub-expressions.

    This optimization has significant runtime cost, so we only do it on
    sub-expressions that are of a certain size.  This is a very rough
    heuristic and can almost certainly be improved upon.
-}
shatterSpine :: Run -> (Prog, SmallArray Run)
shatterSpine top =
    (PROG{arity,varsSz,prgrm}, args)
  where
    args           = smallArrayFromList (either ARG VAR <$> freeRefs)
    freeRefs       = freeVarsForRun top
    argsMap        = mapFromList (zip freeRefs [1..]) -- arg 0 is self
    (prgrm, nmVar) = spineFragment argsMap top
    varsSz         = nmVar + 1 -- TODO: why +1 ?  Not needed?
    arity          = length argsMap

{-
    {spineFragment} is just the main loop of {shatterSpine}.

    `freeTable` is the mapping from (ARG or VAR) into the host stack
    into references into the local stack.

    TODO: BADLY need to switch to SYB or similar here.  The boilerplate
    is absurd.
-}
spineFragment :: Map (Either Int Int) Int -> Run -> (Run, Int)
spineFragment freeTable =
    \top -> runState (go (0,mempty) top) 0
  where
    go :: (Int, Map Int Int) -> Run -> State Int Run
    go z@(next, loc) = \case
        CNS c            -> pure (CNS c)
        SEQ a b          -> SEQ <$> go z a <*> go z b
        TRK a b          -> TRK <$> go z a <*> go z b
        KAL xs           -> KAL <$> for xs (go z)
        PAR r xs         -> PAR r <$> for xs (go z)
        REC xs           -> REC <$> for xs (go z)
        MK_ROW xs        -> MK_ROW <$> for xs (go z)
        MK_TAB xs        -> MK_TAB <$> for xs (go z)
        LAZ prg xs       -> LAZ prg <$> for xs (go z)
        SWI c f v        -> SWI <$> go z c <*> go z f <*> for v (go z)
        JMP c f vs       -> JMP <$> go z c <*> go z f <*> for vs (go z)
        JMP_WORD c f k v -> JMP_WORD <$> go z c <*> go z f <*> pure k <*> for v (go z)
        OP2 f o a b      -> OP2 f o <$> go z a <*> go z b
        EXE x s f xs     -> EXE x s f <$> for xs (go z)
        IF_ i t e        -> IF_ <$> go z i <*> go z t <*> go z e
        IFZ i t e        -> IFZ <$> go z i <*> go z t <*> go z e

        LET i v b        -> do
            let z' = (next+1, insertMap i next loc)
            modify' (max next)
            LET next <$> go z v <*> go z' b

        LETREC vs b -> do
            let step = \((i,_),ix) -> insertMap i (next+ix)
            let loc' = foldr step loc $ zip (toList vs) [0..]
            let z'   = (next + length vs, loc')
            modify' (max next)
            LETREC <$> for vs (traverse (go z')) <*> go z' b

        ARG 0 -> pure (ARG 0)
        ARG x -> pure $ ARG
                      $ fromMaybe (error "spineFragment: impossible")
                      $ lookup (Left x)
                      $ freeTable

        VAR x -> case (lookup (Right x) freeTable, lookup x loc) of
                   (Just f, _) -> pure (ARG f)
                   (_, Just l) -> pure (VAR l)
                   _           -> error "spineFragment: impossible"
