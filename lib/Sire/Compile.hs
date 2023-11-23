-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{- |

    This is a single sire-to-sire transformation whose output has these
    properties:

    -   No function has free variables, including nested lambdas.

            x&(y&x)  ==>  x & ((x y & x) x)

    -   All let bindings are referenced at least twice.

            (x@(add 2 3))(inc x)  ==> (inc (add 2 3))

    -   There are no let bindings whose value expressions are constants.

            (x@3)[x x]  ==>  [3 3]

    -   All requested inline-applications have been performed.

            (**I (inc 3))                 ==>  (inc 3)

            (else 3)                      ==>  (inc 3)

            (I @ (**I x ? x))(I (inc 3))  ==>  (inc 3)

    -   There are no trivial rebindings:

            (i@inc)(x @ i 3)(y@x)(add y y)

                ==>

            (x @ inc 3)(add x x)

-}

-- TODO: Do constant propagation earlier, so that the "no constant
-- bindings" optimization applies to them.  For example:
--
--     @ x [3 4]
--     | foo (idx 0 x) (idx 1 x)
--
-- This should be optimized to:
--
--     | foo (idx 0 [3 4]) (idx 1 [3 4])
--
-- However, we only "flatten constants" during the translation to code.
-- If we somehow do it earlier, this will happen automatically.
--


-- TODO: Lambda lifting of lambdas with no free variables creates new
--       opportunities for constant propagation.  Take advantage?


{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile (compileSire) where

import Loot.Backend
import PlunderPrelude
import Rex
import Sire.Types

import Data.Containers.ListUtils (nubIntOn)
import Data.List                 ((!!))
import Loot.Types                (Bod(..), LawName(LN), Rul(..), Val(..))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Fan      as F


-- Utils -----------------------------------------------------------------------

{-
    `migrate` copies an expression into an sub expression, and rewrites
    de-bruijn indices so that everything remains consistent.

    Bound variables do not change, but free variables need to be increased
    to skip over any bindings that happened in between.  For example:

        @ three | 3
        @ foo3  | (self foo ? [foo three])
        @ four  | 4
        | **k3 9

    The de-bruijn index of three in foo3 is 3:

        ~[foo self foo3 three]

    When we inline k3 we get

        @ three | 3
        @ foo3  | (self foo ? [foo three])
        @ four  | 4
        @ self  | 0
        @ foo   | 9
        | [foo three]

    Here, the de-bruijn index of foo is the same (because it's local),
    but the index of three is now 4:

        ~[foo self four foo3 three]

    We can calculate this by simply increasing the index of each
    free-reference by "how deep in the stack" the thing we are referencing
    is.  In this case, `foo3` was at index=1, so we increase the index of
    `three` by one.
-}
migrate :: Nat -> Nat -> Sire -> Sire
migrate offset alreadyBound topExp =
    if offset==0 then topExp else go alreadyBound topExp
  where
    go :: Nat -> Sire -> Sire
    go l e = case e of
       S_VAR v | v >= l -> S_VAR (v+offset)
       S_VAR{}          -> e
       S_VAL{}          -> e
       S_GLO{}          -> e
       S_LIN x          -> S_LIN (go l x)
       S_APP f x        -> S_APP (go l f) (go l x)
       S_LET v b        -> S_LET (go (l+1) v) (go (l+1) b)
       S_LAM lam        -> S_LAM (lam { body = go ll lam.body })
                             where ll = l + 1 + lam.args

{-
    Checks if a lambda contains any self-references.  Lambdas with
    self-references cannot be inlined.
-}
isRecursive :: Lam -> Bool
isRecursive topLam = references topLam.args topLam.body

references :: Nat -> Sire -> Bool
references d = \case
    S_VAR v   -> v==d
    S_VAL{}   -> False
    S_GLO{}   -> False
    S_LET v b -> references (d+1) v || references (d+1) b
    S_APP f x -> references d f     || references d x
    S_LIN x   -> references d x
    S_LAM l   -> references (d + 1 + l.args) l.body



-- Inline Application ----------------------------------------------------------

inlineExp
    :: [Maybe Sire]
    -> (Sire, [Sire])
    -> Either Text (Sire, [Sire])
inlineExp locals (f,xs) = case f of
    S_LAM l -> (fn 0) l
    S_GLO b -> maybe who (fn 0) (getLam b.d.code)
    S_VAR v -> maybe who (fn v) (getLam =<< (locals !! fromIntegral v))
    _       -> who
  where
    who = Left "Not a known function"

    getLam (S_LAM s) = Just s
    getLam (S_GLO b) = getLam b.d.code
    getLam (S_LIN b) = getLam b
    getLam _         = Nothing

    fn :: Nat -> Lam -> Either Text (Sire, [Sire])
    fn depth func = do
        let arity     = fromIntegral func.args
        let argExps   = take arity xs
        let overflow  = drop arity xs
        let numParams = length xs

        when (numParams < arity || isRecursive func) do
            Left "too few params or is recursive"

        pure $ (,overflow)
             $ bindArgs argExps
             $ migrate depth (fromIntegral $ arity+1) func.body

    -- The (S_VAL 0) is a dummy-binder for self-reference.  It will be
    -- optimized away by a later pass.  We know that this dummy is never
    -- referenced referenced because we already checked that the
    -- function was non-recursive before we inlined it.
    bindArgs :: [Sire] -> Sire -> Sire
    bindArgs args body = foldr S_LET body $ renumber 1 (S_VAL 0 : args)

    -- Each argument is bound as a LET, so the self-ref is in scope,
    -- as is every previous binding.
    renumber :: Nat -> [Sire] -> [Sire]
    renumber _ []     = []
    renumber n (s:ss) = migrate n 0 s : renumber (n+1) ss



-- Inlining --------------------------------------------------------------------

{-
    Things that should be auto-inlined.

    1)  Reference to marked global bindings.

        | *else 3

    2)  Direct calls to marked functions

        | (**_ x & x) 3

    3)  Reference to locally-bound marked functions

        @ I (**I x ? x)
        | I 3

    4)  Reference to locally-bound marked-functions via chain of
        references.

        @ I (**I x ? x)
        @ i I
        | I 3
-}
markThingsToBeInlined :: Sire -> Sire
markThingsToBeInlined =
    snd . go []
  where
    mark :: Sire -> Sire
    mark (S_LIN x) = mark x
    mark x         = S_LIN x

    go :: [Bool] -> Sire -> (Bool, Sire)
    go m = \case
        x@S_VAL{} -> (False, x)
        S_APP f x -> (False, S_APP (snd $ go m f) (snd $ go m x))
        S_LET v b -> goLet m v b
        S_VAR r   -> goVar m r
        S_LIN f   -> mark <$> go m f
        S_GLO b   -> goGlo b
        S_LAM l   -> goLam m l

    globalIsMarked :: Binding -> Bool
    globalIsMarked b =
        case b.d.code of
            S_LAM l -> l.inline
            _       -> False

    goVar m r | m !! fromIntegral r = (True,)  $ S_LIN (S_VAR r)
    goVar _ r | otherwise           = (False,) $ S_VAR r

    goGlo g | globalIsMarked g = (True,  S_LIN (S_GLO g))
    goGlo g | otherwise        = (False, S_GLO g)

    goLam m lam =
        if lam.inline then (True, S_LIN e) else (False, e)
      where
        m2 = replicate (fromIntegral (1 + lam.args)) False <> m
        e = S_LAM lam{ body = snd (go m2 lam.body) }

    -- You can't inline something by self-reference through a letrec
    -- binding, so we first process the binding with self-referenced
    -- unmarked, and then we use the "is marked" flag of the result to
    -- process the body.
    goLet m v b =
        let
            (varIsMarked,  v2) = go (False : m) v
            (bodyIsMarked, b2) = go (varIsMarked : m) b
        in
            (bodyIsMarked, S_LET v2 b2)


-- Inline All ------------------------------------------------------------------

{-
    Inlines every (**f x1 .. xn) where arity(f)=n and f is one of these:

    -   A function literal.
    -   A reference to a global function.
    -   A reference to a local function.
-}
inlineAll :: Sire -> Sire
inlineAll topSire =
     go [] (topSire, [])
  where
    go :: [Maybe Sire] -> (Sire, [Sire]) -> Sire
    go s (h, xs) = case h of
        x@S_VAL{}     -> apple x xs
        x@S_GLO{}     -> apple x xs
        x@S_VAR{}     -> apple x xs
        S_APP f x     -> let x2 = go s (x,[]) in go s (f, x2:xs)
        S_LET v b     -> apple (doLet s v b) xs
        S_LAM f       -> apple (S_LAM $ doLam s f) xs
        S_LIN S_LIN{} -> error "Internal Error:  doubled inline marker"
        S_LIN x       -> doInline s (x,xs)

    -- If we can't inline, we still apply the inline transformation to
    -- the heade head.  Also, an unapplicable S_LIN annotation does not
    -- remove the S_LIN optimization.  If this code is inlined into
    -- other code, that can create new opportunities for inlining.
    --
    -- If we successfully inline, we re-process the result to see if
    -- that brings forth more opportunities to inline.
    doInline :: [Maybe Sire] -> (Sire, [Sire]) -> Sire
    doInline s (h,xs) =
         case inlineExp s (h,xs) of
             Left _reason ->
                 apple (S_LIN h2) xs where h2 = go s (h, [])
             Right (newExp, extras) ->
                 go s (newExp, extras)

    -- Self-reference and arguments can never be inlined, so we just
    -- bind them all as nothing values.
    doLam :: [Maybe Sire] -> Lam -> Lam
    doLam scope lam =
        lam{body=newBody}
      where
        scope2  = replicate (fromIntegral(lam.args + 1)) Nothing <> scope
        newBody = go scope2 (lam.body, [])

    doLet :: [Maybe Sire] -> Sire -> Sire -> Sire
    doLet l v b =
        S_LET newV newB
      where
        newV = go (Nothing   : l) (v, [])
        newB = go (Just newV : l) (b, [])


-- Lambda Lifting --------------------------------------------------------------

{-
    To lambda lift a function

    1)  Walk the body and lambda lift each lambda found there.

    2)  Calculate the set of free variables

    ... TODO
-}
lambdaLift :: Sire -> Sire
lambdaLift = goE
  where
    goE :: Sire -> Sire
    goE = \case
        S_LAM f     -> lambdaLiftLaw $ f { body = goE f.body }
        S_APP f x   -> S_APP (goE f) (goE x)
        S_LIN x     -> S_LIN (goE x)
        S_LET v b   -> S_LET (goE v) (goE b)
        x@S_VAR{}   -> x
        x@S_VAL{}   -> x
        x@S_GLO{}   -> x


{-
    {lambdaLiftLaw} Expects that all functions in the body are already
    lifted.  In other words, lifting must be applied to all sub-functions
    before the contained function may be lifted.

    If there are no free variables, then there we just return the input
    without modification.

    Otherwise, we:

    1.  Add each free variable to the front of the argument list.

    2.  Rebind self-reference to be the new function applied to each
        lifted argument.

    3.  Rewrite free variable-references to reflect the new structure.

    For example:

        @ x 3
        @ y 4
        | (foo z ? [x y z])

    Becomes:

        @ x 3
        @ y 4
        @ a 5
        ^ _ x y
        ? "foo" (foo_ x y z)
        @ foo (foo_ x y)
        | [x y z]

    Previously the scope was:

        ~[z foo a y x]

    But now the scope is:

        ~[foo z y x foo_ a y x]

    This is a fairly complicated reordering.

    1.  Besides self-reference, each argument is bumped up by one.

    2.  Self-reference is decreased by 1 (the number of normal arguments)

    3.  Free variables are now (depth + freeVarIdx + numArgs + 1)

    How is this implemented?

        Create a table for the rewrite at the top of the function scope:

            ## 0=1 ;; z:   args are incremented.
            ## 1=0 ;; foo: self-reference is always zero
            ## 3=2 ;; y:   (numArgs + (num_lifted - lifted_index))
            ## 4=3 ;; x:   (numArgs + (num_lifted - lifted_index))

        Then walk the body, keeping track of the number of local bindings
        (since the function itself).

        -   If a binding is local, leave it alone.
        -   If a binding is not local, subtract the number of locals
            and lookup in the pre-computed table.

    Example:

        num_args = 3
        lifted   = [5 7 9]

        table = {
            0: 1   -- args (n -> n+1)
            1: 2   -- args (n -> n+1)
            2: 3   -- args (n -> n+1)
            3: 0   -- self  (numArgs -> 0)
            9: 4   -- numArgs+1 (backwards traversal over lifted list)
            7: 5   -- numArgs+2 (backwards traversal over lifted list)
            5: 6   -- numArgs+3 (backwards traversal over lifted list)
        }

    Simplification:

        We can use a table only for the lifted arguments:

            9 -> 4   -- numArgs+1 (backwards traversal over lifted list)
            7 -> 5   -- numArgs+2 (backwards traversal over lifted list)
            5 -> 6   -- numArgs+3 (backwards traversal over lifted list)

        And just use code for the rest:

            table = mapFromList $ zip (reverse free) [numArgs+1 ..]

            rewrite n | n < locals = n
            rewrite n              = case compare ref numArgs of
                                         LT -> n+1
                                         EQ -> 0
                                         GT -> table[n]

    And what is the local rebinding?

        num_args = 3
        lifted   = [5 7 9]

        ? (liftedSelf _5 _7 _9 a b c)
        @ new_self (liftedSelf _5 _7 _9)

        liftedSelfRef = numArgs + numLifted + 1

        apple_ ((liftedSelfRef -) <$> [0..numLifted])
-}
lambdaLiftLaw :: Lam -> Sire
lambdaLiftLaw lam =
    -- trace ("table:" <> show (mapToList table)) $
    -- trace ("free:" <> show free) $
    -- trace ("newNumArgs:" <> show newNumArgs) $
    -- trace ("lam.args:" <> show lam.args) $
    if null free
    then S_LAM lam
    else apple (S_LAM lifted) (S_VAR <$> free)
  where
    free :: [Nat]
    free = freeVars lam

    numFree    = length free
    newNumArgs = lam.args + fromIntegral numFree
    selfRefIdx = numFree + fromIntegral lam.args + 1

    lifted = lam { args = newNumArgs, body = liftedBody }

    newSelf    = apple_ (S_VAR . fromIntegral . (selfRefIdx -) <$> [0..numFree])
    liftedBody = S_LET newSelf (liftBod 0 lam.body)

    -- Apply `rewrite` to each non-locally-bound reference.
    liftBod :: Nat -> Sire -> Sire
    liftBod nb e = case e of
        S_VAR n | n < nb -> S_VAR n
        S_VAR n          -> S_VAR ((rewrite(n-nb)) + nb)
        S_LIN x          -> S_LIN (liftBod nb x)
        S_APP f x        -> S_APP (liftBod nb f) (liftBod nb x)
        S_LET v b        -> S_LET (liftBod (nb+1) v) (liftBod (nb+1) b)
        S_VAL{}          -> e
        S_GLO{}          -> e
        S_LAM{}          -> e -- already lifted, no free vars.

    -- Map from free variable to their new locations in the argument list.
    table :: Map Nat Nat
    table = mapFromList $ zip (reverse free) [lam.args+1 ..]

    -- Handles the rearrangement of self reference, normal arguments, and
    -- lifted arguments.
    rewrite :: Nat -> Nat
    rewrite n =
        let
            bad = "impossible: bad rewrite table in lambdaLiftLaw.\n" <>
                  "    Missing: " <> show key

            key = n - (lam.args + 1)

            new =
                case compare n lam.args of
                    LT -> n+1                                     --  normal arg
                    EQ -> 0                                       --  self
                    GT -> fromMaybe (error bad) $ lookup key table  --  lifted arg
        in
            new
            -- trace ("rewrite " <> show n <> " -> " <> show new) new

freeVars :: Lam -> [Nat]
freeVars topLaw =
    nubIntOn fromIntegral $ go (topLaw.args + 1) topLaw.body
  where
    go :: Nat -> Sire -> [Nat]
    go !nb = \case
        S_VAL{}     -> []
        S_GLO{}     -> []
        S_VAR r     -> if r<nb then [] else [r-nb]
        S_APP f x   -> go nb f <> go nb x
        S_LIN f     -> go nb f
        S_LET v b   -> go (nb+1) v <> go (nb+1) b
        S_LAM f     -> go (nb + 1 + f.args) f.body


-- Eliminate Binds to constant values ------------------------------------------

{-
    If any binding points to a constant values (a number, a constant
    fan value, or a global), this replaces all references to that binding
    with the constant.

    This optimization creates unused bindings, that must be removed by
    a later pass.

    For example.  This:

        @ x 3
        @ y add
        | [x y]

    Becomes this:

        @ x 3
        @ y add
        | [3 add]
-}
eliminateConstantBindings :: Sire -> Sire
eliminateConstantBindings = go []
  where
    go :: [Maybe Sire] -> Sire -> Sire
    go e = \case
        x@S_VAL{}   -> x
        x@S_GLO{}   -> x
        S_APP f x   -> S_APP (go e f) (go e x)
        S_LAM f     -> let e2 = replicateNat (f.args+1) Nothing <> e
                       in S_LAM f{body = go e2 f.body}
        S_LIN x     -> S_LIN (go e x)
        x@(S_VAR v) -> fromMaybe x (ix "var" e $ fromIntegral v)
        S_LET x b   -> S_LET (go e2 x)
                             (go (getConst e3 x : e) b)
                         where
                           e2 = Nothing       : e
                           e3 = getConst e2 x : e

    getConst :: [Maybe Sire] -> Sire -> Maybe Sire
    getConst e = \case
        x@S_VAL{} -> pure x
        x@S_GLO{} -> pure x
        S_APP{}   -> Nothing
        S_LET{}   -> Nothing
        S_LAM{}   -> Nothing
        S_LIN x   -> getConst e x
        S_VAR v   -> ix "getConst" e (fromIntegral v)

ix :: Show a => String -> [a] -> Int -> a
ix ctx ls n =
    if n >= length ls
    then error ("(" <> ctx <> ")indexing " <> show n <> " into " <> show ls)
    else ls !! n

replicateNat :: Nat -> a -> [a]
replicateNat n x = replicate (fromIntegral n) x


-- Eliminate bindings that are used exactly once (except nested functions) -----

{-
    For each binding that is used exactly once, we replace the reference
    to the binding, with the actual expression of the binding.  For example:

        @ x (add 3 4)
        @ y (inc 3)
        | add x y

    Becomes:

        @ x (add 3 4)
        @ y (inc 3)
        | add (add 3 4) y

    This optimization creates unused bindings, so it's important that
    this happen before `eliminateUnusedBindings`

    ----

    If a binding is referenced from within a nested function, this
    optimization must not apply, since that can have significant impact
    on the lazy-evaluation behavior.  For example, this:

        @ x (Ackermann 3 3)
        ? (addAcker n)
        | add x n

    Would become this:

        ? (addAcker n)
        | add (Ackermann 3 3) n

    Which calls `Ackermann` every time, instead of just once.
-}
eliminateSingleUseBindings :: Sire -> Sire
eliminateSingleUseBindings =
    snd . go 0
  where
    go :: Nat -> Sire -> (Map Nat Nat, Sire)
    go d = \case
      x@S_VAL{}   -> (mempty, x)
      x@S_GLO{}   -> (mempty, x)
      x@(S_VAR v) -> (M.singleton (d - (v+1)) 1, x)
      S_APP x y   -> let (ux, sx) = go d x
                         (uy, sy) = go d y
                     in (M.unionWith (+) ux uy, S_APP sx sy)
      S_LIN x     -> S_LIN <$> go d x
      S_LAM f     -> (ub, S_LAM f{body=b2})
                       where (ub, b2) = go (d + f.args + 1) f.body
      S_LET v b -> goLet d v b

    goLet :: Nat -> Sire -> Sire -> (Map Nat Nat, Sire)
    goLet d v b =
        (M.unionWith (+) uv ub,)
            $ S_LET v2
            $ if not recursive && useCount == 1
              then rewrite 0 v2 b2
              else b2
      where
        (uv, v2)  = go (d+1) v
        (ub, b2)  = go (d+1) b
        recursive = M.member d uv
        useCount  = fromMaybe 0 (lookup d ub)

    rewrite :: Nat -> Sire -> Sire -> Sire
    rewrite z k = \case
        S_VAR v | v==z -> migrate z 0 k
        x@S_VAR{}      -> x
        x@S_LAM{}      -> x -- Do not apply to nested lambdas
        x@S_GLO{}      -> x
        x@S_VAL{}      -> x
        S_APP f x      -> S_APP (rewrite z k f) (rewrite z k x)
        S_LIN x        -> S_LIN (rewrite z k x)
        S_LET v b      -> S_LET (rewrite (z+1) k v) (rewrite (z+1) k b)


-- Eliminate Simple Rebinds ----------------------------------------------------

{-
    If a binding is a simple rebinding of another binding, rewrite
    all reference to that binding to refer to the original binding.

    For example:

        (@ 3)(@ 4)(@ $1)(@ $1)[$0 $1 $2 $3]

   Is rewritten to:

        (@ 3)(@ 4)(@ $1)(@ $1)[$2 $2 $2 $3]

    This optimization creates unused bindings, so it's important that
    eliminateUnusedBindings happens after this pass.

    When the algorithm hits the body in the example above, `e` has the
    following value.

       [2, 1, 0, 0]

    This is used as a table of offsets, used to rewrite each binding.
-}
eliminateRedundantBindings :: Sire -> Sire
eliminateRedundantBindings = go []
  where
    go :: [Nat] -> Sire -> Sire
    go e = \case
        x@S_VAL{} -> x
        x@S_GLO{} -> x
        S_APP f x -> S_APP (go e f) (go e x)
        S_LIN x   -> S_LIN (go e x)
        S_VAR v   -> S_VAR (v + (e !! fromIntegral v))

        S_LAM f   -> S_LAM f{body = go e2 f.body}
                       where e2 = replicate (fromIntegral f.args + 1) 0 <> e

        S_LET v b -> S_LET v2 (go (getOff v2 : e) b)
                       where v2 = go (0:e) v

    -- This is run against the already-processed binding expression,
    -- so if we see a variable it has already been mapped to its
    -- original binding.
    getOff (S_VAR v) = v
    getOff (S_LIN x) = getOff x -- TODO should we both to handle this case?
    getOff _         = 0


-- Eliminate Unused Bindings ---------------------------------------------------


{-
    This eliminates all let bindings that are not used, including bindings
    that are only used by other bindings that are not used.

    The main `go`, returns an expression and the set of bindings used
    within that expression.

    Since de-bruijn indicies are relative the their position, we convert
    them into offsets from the top-most position by pre-calculating the
    total depth.

        topOffset = currentDepth - dbIdx

    topOffset is not unique accross the whole tree, but the offset that
    we care about is always coherent.  For example:

        @ _v0 3
        | (@ _v1 3)_v1
          (@ _v1 4)5

    The set that we examine to determine _v0 is {1}, but what is that
    `1` refering to?  The answer is ("is any binding used at depth 1"),
    which is useless, but we don't care, because we are only checking
    to see if offset=0 exists.

    If we detect that a binding is not used, then we omit it.

    When we omit a binding, the de-bruijn indicies of the references in
    the body of the ommitted binding must be rewritten.  Specifically,
    all "free variables" (variables that refer to things not bound within
    the binding body) must be decremented.

    For example, this:

        (@ 3)(@ 4)(@ 5)[$0 $2]

    Becomes this:

        (@ 3)(@ 5)[$0 $1]
-}
eliminateUnusedBindings :: Sire -> Sire
eliminateUnusedBindings =
    snd . go 0
  where
    go :: Nat -> Sire -> (Set Nat, Sire)
    go d = \case
        x@S_VAL{}   -> (mempty, x)
        x@(S_VAR v) -> (singleton (d-(v+1)), x)
        x@S_GLO{}   -> (mempty, x)
        S_APP x y   -> (S.union xu yu, S_APP xs ys)
                         where (xu,xs) = go d x
                               (yu,ys) = go d y
        S_LAM f     -> let (bu, bs) = go (d+1+f.args) f.body
                       in (bu, S_LAM f{body = bs})

        S_LIN x     -> S_LIN <$> go d x

        S_LET v b ->
              if member d bu
              then (union vu bu, S_LET vs bs)
              else (bu, rewrite 0 bs)
            where
              (vu, vs) = go (d+1) v
              (bu, bs) = go (d+1) b

    rewrite :: Nat -> Sire -> Sire
    rewrite l = \case
        S_APP f x        -> S_APP (rewrite l f) (rewrite l x)
        S_LAM f          -> S_LAM f{body = rewrite (f.args+1+l) f.body}
        S_LIN x          -> S_LIN (rewrite l x)
        S_LET v b        -> S_LET (rewrite (l+1) v) (rewrite (l+1) b)
        x@S_VAL{}        -> x
        x@S_GLO{}        -> x
        S_VAR v          -> case compare v l of
                                LT -> S_VAR v
                                EQ -> error "impossible: unused var is used"
                                GT -> S_VAR (v-1)


-- Sire Transformation ---------------------------------------------------------


-- TODO Avoid running `eliminateUnusedBindings` many times.  But, how?
-- Have the other passes eliminate their bindings?
--
-- TODO Avoid running `eliminateUnusedBindings` many times.

inline :: Sire -> Sire
inline =
    ( traceSireId "inlined"
    . inlineAll
    . traceSireId "marked"
    . markThingsToBeInlined
    )

optimize :: Sire -> Sire
optimize =
    ( traceSireId "no single-use bindings"
    . eliminateUnusedBindings
    . eliminateSingleUseBindings
    . traceSireId "no redundant re-bindings"
    . eliminateUnusedBindings
    . eliminateRedundantBindings
    . traceSireId "no constant bindings"
    . eliminateUnusedBindings
    . eliminateConstantBindings
    . traceSireId "no unused bindings"
    . eliminateUnusedBindings
    )

transformSire :: Sire -> Sire
transformSire =
    ( traceSireId "FINISH HIM"
    . optimize
    . traceSireId "lifted"
    . lambdaLift
    . optimize
    . inline
    )


-- Internal Types --------------------------------------------------------------

{-
    The compilation result:

    -   [args]: If the expression is a constant, this is the number
        of extra arguments that can be applied before the expression is
        no longer constant.  If the expression is not constant, this is
        set to 0.

    -   [code]: The actual PLAN law-code that this compiles into.
-}
data CRes = CR
    { arity  :: Nat
    , code   :: Bod Fan
    }
  deriving (Eq, Show)


-- Sire Expression to Law Body -------------------------------------------------

{-
    This compiles a nested function directly into a PLAN value.

    We assume that the input contains no free variables, a property
    that is guaranteed by `transformSire`.
-}
compileFun :: Lam -> Fan
compileFun lam =
    if False then
        trk (F.REX rex) res
    else
        res
  where
    rex = N OPEN "#" [T TEXT "compileFun" Nothing]
        $ Just (lamRex lam)

    res = if lam.pin then F.mkPin fanVal else fanVal
    fanVal = ruleFanOpt
                 $ RUL (LN lam.tag) lam.args
                 $ (.code)
                 $ expBod (lam.args + 1) lam.body

{-
    This compiles a Sire expression into PLAN legal code.  If the result
    is a constant value, we track the arity, otherwise we set arity=0.

    We use the arity information to optimize code size.  For example,
    the input expression `(0 0)` could compile to this:

        (0 (2 0) (2 0))

    Or it could compile to this:

        (2 (0 0))

    The latter is preferable, but this merger is only possible because
    the head (0) has arity>1, otherwise we would be moving evaluation
    from runtime to compile time.

    TODO: This would actually be a lot simpler if we could cut Loot.Val
    out of the loop and just use Fan values.  In particular, there expBod
    would no longer need to track arities, since we can just use
    `F.trueArity` directly.
-}
expBod :: Nat -> Sire -> CRes
expBod d = \case

    S_VAL pln ->
        CR{arity,code}
      where
        arity = fromIntegral (F.trueArity pln)
        code  = BCNS $ REF pln

    S_LAM f -> expBod d $ S_VAL $ compileFun f
    S_VAR r -> CR 0 (BVAR (d - (r+1)))

    S_GLO bind ->
        CR{arity,code}
      where
        arity = fromIntegral (F.trueArity bind.d.value)
        code  = BCNS (REF bind.d.value)

    S_APP f x -> bapp (expBod d f) (expBod d x)
    S_LIN f   -> expBod d f

    S_LET v b ->
        CR { arity = b2.arity, code = BLET v2.code b2.code }
      where
        v2 = expBod (d+1) v
        b2 = expBod (d+1) b

  where
    bapp :: CRes -> CRes -> CRes
    bapp f x =
        case (f.code, x.code) of
            (BCNS a, BCNS b) | f.arity>1 -> cnsApp (f.arity - 1) a b
            _                            -> dynApp (f.arity - 1)
      where
        dynApp arity     = CR arity (BAPP f.code x.code)
        cnsApp arity a b = CR arity (BCNS $ APP a b)


-- The Sire Compiler -----------------------------------------------------------

compileSire :: Sire -> Fan
compileSire sire =
    traceSire "compileSire" sire $
    let
        res = transformSire sire
        rul = RUL (LN 0) 1 $ (.code) $ expBod 2 res
                --  The `2` argument to `expBod` is because
                --  self-reference is bound for this dummy rule
                --  and the rule takes one dummy argument.
    in
        traceSire "comipleSire(input)" sire $
            traceSire "comipleSire(output)" res $
                (ruleFanOpt rul F.%% 0)
