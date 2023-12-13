# The Sire Backend

This is a commentary on the implementation of the Sire backend.

For documentation on the overall design of the Sire inliner and
code generator, see [SIRE_INLINE.md](SIRE_INLINE.md) and
[SIRE_CODEGEN.md](SIRE_CODEGEN.md).

## External Types

These types are produced by the front-end.

### The `Sire` Type

This represents Sire ASTs after name-resolution.

``` haskell
data Sire
    = V Nat
    | K Fan
    | G Binding
    | A Sire Sire
    | L Sire Sire
    | M Sire
    | F Lam
```

-   `V` (VARIABLE) is a local variable reference, using de bruijn
    indices.

-   `K` (CONSTANT) is a constant value embedded into the code
    (number literal, or constants produced by macros)

-   `G` (GLOBAL) is a reference to a global variable binding,
    this will be discussed in more detail shortly.

-   `A` (APPLY) is function application.

-   `L` (LET) is a let-binding. The expression to bind and the
    body. This doesn't have a "variable name" slot because,
    again, we are using de-bruijn indices.

-   `M` (MARK) is an inline-marker: The expression `**3` is
    stored as `(M (K 3))`.

-   `F` (FUNCTION) is a nested lambda expression.

### De Bruijn Indices

"De Bruijn Indices" is a scheme where variables refer to bindings
by "distance". `(V 0)` refers nearest binding, `(V 1)` is the
binding outside of that, etc.

For example:

    @ a 6
    @ b 7
    @ c 9
    | a b c

Is encoded as:

    @ 6
    @ 7
    @ 9
    | $2 $1 $0

This is an extremely simple and convenient encoding.

The upside of this representation is that we don't have to deal
with names or binding keys, and we don't need to generate new
binding names/keys when we create new bindings.

This is especially important because the `Sire` datatype is
stored in bindings in the Sire state. So this representation is
not an implementation detail, it's part of the specified behavior
of Sire. If we used unique keys, then all of the details of the
unique supply would need to become a part of the Sire spec.

The downside of this approach is that writing transformations
within this representation can be unintuitive, and it's
notoriously easy to make mistakes. However, we only do one
transformation within this representation, it's simple, and it's
frozen so it's not subject to churn. Because of this, this
downside doesn't affect us much.

### The `Lam` Type

This represents a Lambda within a Sire AST.

``` haskell
data Lam = LAM
    { pin    :: Bool
    , inline :: Bool
    , tag    :: Nat
    , args   :: Nat
    , body   :: Sire
    }
```

-   `pin` indicates whether or not the resulting law should be
    pinned. This is true if the input function was created using
    the `??` rune.

-   `inline` indicate where the lambda was marked with an "always
    inline" annotation. `(**const x y)=x` for example.

-   `tag` indicates the name that should be tagged onto the PLAN
    law that is generated from this code.

-   `args` is the arity of the function.

-   `body` is the code for the function. This body may contain
    references to bindings defined outside of the function ("free
    variables"), and those will be resolved via lambda lifting.

### Bindings

The `Binding` type represents all of the data associated with a
global binding. This records the value, the name, the module, and
a unique key for every binding. It also records the `Sire` AST
that was used when creating the binding, which we use for
inlining.

``` haskell
data Binding = BINDING
    { key      :: Nat
    , value    :: Fan
    , code     :: Sire
    , location :: Fan
    , name     :: Fan
    }
```

In the Haskell code, you will instead see this as the
`BindingData` type, which is conceptually the same as the above,
but which is slightly more complicated due to concerns that are
not relevant here.

### Encoding as PLAN

Since these types are accessible in the Sire state, their PLAN
representation needs to be specified.

`Sire` trees are encoded as:

    (V v)   = [%V v]
    (K x)   = [%K x]
    (G b)   = [%G b]
    (A f x) = [%A f x]
    (L v b) = [%L v b]
    (M x)   = [%M x]
    (F fn)  = [%F fn]

A `Lam` record is just an array:

    [pin inline tag args body]

A `Binding` is also an array, but it's always wrapped in a pin.

    (4 [key value code module name])

Bindings need to be pinned, because bindings contain code, and
code contains bindings. The result is a massively duplicated
tree, and we need the pin-wrapper in order to be able to
efficiently serialize bindings (which we need to do for build
caching).

## Inlining

The entire inliner fits on a single page, but it's pretty dense
code. In this section we'll do a slow walk through the code and
explain every aspect of it.

### The `Arg` type

This is an expression tagged with the nesting depth that it was
found within. This depth information is needed because inlining
will cause expressions to be relocated, and that requires us to
renumber the de bruijn indices. More on that later.

``` haskell
data Arg = ARG
    { d :: Int
    , x :: Sire
    }
```

### The `Pot` type

``` haskell
data Pot = POT
    { lam    :: Lam
    , marked :: Bool
    , deep   :: Int
    , need   :: Int
    , args   :: [Arg]
    }
```

The `Pot` type represents the "inlining potential" of an
expression. In order to be inlined, an expression must:

-   Contain a lambda at it's head. (`lam`)

-   Be marked with an inline annotation. (`marked`)

-   And have an parameter to fill each of the arguments of the
    lambda (`args`).

In addition to this:

-   We track `deep`, the depth at which we found the lambda
    (mirroring `Xarg.d`).

-   And we track `need` (the number of remaining arguments needed
    before an inlining can take place)

Inline expansion can be performed on an `Pot` e if `e.marked` is
set and `e.need` is 0.

### The `Res` type

``` haskell
data Res = RES
    { out :: Sire
    , pot :: Maybe Pot
    }
```

The inliner returns an `Res`, which includes `(.out)` the result
of the pass, and `(.pot)`, which is the "inline potential" of the
expression.

For example, this input:

``` haskell
inline [sire| (**const 3) |]
```

Would produce:

``` haskell
RES
    { out = A (M (G const)) (K 3)
    , pot = Just POT
        { lam    = LAM {pin=True, inline=False, tag="const", args=2, body=(V 1)}
        , marked = True
        , deep   = 0
        , need   = 1
        , args   = ARG 0 (K 3)
        }
    }
```

Note that no inlining has been performed, but the result records
that there is "inlining potential". If this was given another
argument, we could do inline expansion.

Similarly, these inputs:

``` haskell
inline [sire| 3           |]
inline [sire| (0 1)       |]
inline [sire| (add 1 2 3) |]
```

Have no inline potential. You can't inline nats, only lambdas.
And `(add 1 2 3)` is already oversaturated but no inline
annotation was applied.

### Utilities for Inlining

First, a couple of utilities.

``` haskell
hasRefTo :: Nat -> Sire -> Bool
hasRefTo d = \case
    V v   -> v==d
    L v b -> hasRefTo (d+1) v || hasRefTo (d+1) b
    A f x -> hasRefTo d f     || hasRefTo d x
    M x   -> hasRefTo d x
    F l   -> hasRefTo (d + 1 + l.args) l.body
    _     -> False
```

This simply asks "does this expression contain a reference to
this variable?" We use this to check if functions (and let
bindings) are recursive, since it isn't safe to perform inline
expansion on recursive functions or bindings.

``` haskell
moveTo :: Int -> Int -> Nat -> Sire -> Sire
moveTo from to alreadyBound topExp =
    if from == to then topExp else go alreadyBound topExp
  where
    go :: Nat -> Sire -> Sire
    go l = \case
        V v | v>=l -> V ((v + fromIntegral to) - fromIntegral from)
        M x        -> M (go l x)
        A f x      -> A (go l f) (go l x)
        L v b      -> L (go (l+1) v) (go (l+1) b)
        F lam      -> F (lam { body = go (l + 1 + lam.args) lam.body })
        e          -> e
```

`moveTo` relocates and expression from a higher nesting context
(at nesting depth `from`) into a more-deeply-nested context (at
nesting depth `to`). This does renumbering of de bruijn indices.

Since we are moving expression "deeper", moving them inside of
additional binders, the de-bruijn indices need to be increased by
a certain offset. However, references to things that are bound
*inside* of the expression that we are moving do not need to
change.

Let's say we have the expression:

    @ a 3
    @ c 5
    | a c

And we want to insert the binding `(@ b 4)` between `a` and `c`.
So, we thing of that as "moving" `(@ c 5)(a c)` "down into" the
`(@ c 5)` binding.

The expression that we want to move is:

    @ 5     ; @ c 5
    | $1 $0 ; | a c

We want to "move that from depth 1 to depth 2" which means that
we increase all "free variables" by 1. The result is:

    @ 5     ; @ c 5
    | $2 $0 ; | a c

Notice that the \$0 didn't change, since it's referring to `c`
which is part of the expression that's being moved.

The full result it:

    @ 3     ; @ a 3
    @ 4     ; @ b 4
    @ 5     ; @ c 5
    | $2 $0 ; | a c

### The Inlining Pass: The Basic Framework

And finally, the inline pass itself:

``` haskell
inline :: Int -> [Maybe Pot] -> [Arg] -> Sire -> Res
inline d env params inp = case inp of
    ...
```

There are a couple of aspects you need to understand before this
code will make sense, the first one is the combination of `env`
and `params`. Let's start by working through an example:

``` haskell
| @ k **const
  @ k3 (k 3)
  | k3
| 4
```

The above example *should* macro-expand, but the pieces needed to
do that are not immediately at hand, the function and it's
arguments are scattered across the input and are obscured by
various intermediate constructs.

In order for the algorithm to make sense of this, it walks the
input, while keeping track of three things:

-   The "inline potential" of each binding (`env`).

-   A list of "pending arguments" (`params`).

-   The current "nesting depth" (`d`, which is used when
    relocating expressions with `moveTo`).

To see what I mean, let's walk through the example step-by-step:

    input=((k @ **const)(k3 @ k 3)k3 4)
    params=[]
    env=[]
    d=0

    input=((k @ **const)(k3 @ k 3)k3)
    params=[4]
    env=[]
    d=0

    input=((k3 @ k 3)k3)
    params=[4]
    env=[k=(**const)]
    d=1

    input=k3
    params=[4]
    env=[k=(**const) k3=(**const 3)]
    d=2

And then here, we re-apply the pending parameters. The "inline
potential" for `k3` is `(**const 3)` and when we apply `4` to
that we get the inline potential `(**const 3 4)`.

This is both saturated and marked for inlining, so inline
expansion is performed.

### Inlining Floats Lets

Note that this system of collecting "pending" arguments and then
"re-applying" them within a let-body has the side-effect of
floating lets outside of applications.

This has no behavioral change, but even more importantly, the
actual code generation pass is going to do this anyways. So, this
effect has no significance at all.

### The Inlining Pass: Case-by-Case

Here is the inlining pass in full:

``` haskell
inline :: Int -> [Maybe Pot] -> [Arg] -> Sire -> Res
inline d env params inp = case inp of
    K _   -> reapply params $ RES inp Nothing
    V v   -> reapply params $ RES inp (env !! fromIntegral v)
    G b   -> reapply params $ RES inp (inline d [] [] b.d.code).pot
    M b   -> reapply params $ RES r (me <&> \e -> e{marked=True})
               where RES r me = inline d env [] b
    F lam -> reapply params $ RES out pot
               where
             need = fromIntegral lam.args
             env' = replicate (1 + need) Nothing <> env
             d'   = 1 + need + d
             out  = F lam{ body = (inline d' env' [] lam.body).out }
             pot  = do guard (not $ hasRefTo lam.args lam.body)
                       Just POT{ lam, marked=lam.inline, deep=d, need, args=[] }
    L v b -> RES (L vr.out br.out) Nothing
               where vr      = inline (d+1) (Nothing  : env) []     v
                     br      = inline (d+1) (noCyc vr : env) params b
                     noCyc x = guard (not $ hasRefTo 0 x.out) >> x.pot
    A f x -> inline d env (ARG d x' : params) f
               where x' = (inline d env [] x).out
  where
    reapply args f@(RES _ me) = case (args, me) of
        (_, Just e) | e.need==0 && e.marked -> inline d env args (expand e)
        ([], _)                             -> f
        (r@(ARG rd rx) : rs, _)             ->
            reapply rs $ RES (A f.out $ moveTo rd d 0 rx) do
                fe <- me
                guard (fe.need > 0)
                pure fe { need = fe.need - 1, args = r : fe.args }

    expand fe = foldr L body $ renum 1 (ARG d (K 0) : reverse fe.args)
                  where body = moveTo fe.deep d (1 + fe.lam.args) fe.lam.body

    renum :: Int -> [Arg] -> [Sire]
    renum _  []     = []
    renum !n (a:as) = moveTo a.d (d+n) 0 a.x : renum (n+1) as
```

This is quite a mouthful, so we'll go through it step by step.

``` haskell
K _ -> reapply params $ RES inp Nothing
```

If we see a constant value there is no inlining potential, just
reapply the pending arguments.

``` haskell
V v -> reapply params $ RES inp (env !! fromIntegral v)
```

If we see a variable reference, the "default" result is just the
expression, and we lookup the "inline potential" (called Pot from
here on) from our table of per-binding potentials.

``` haskell
G b -> reapply params $ RES inp (inline d [] [] b.d.code).pot
```

If we see a global variable, then it's inlining potential can be
found by running the pass against the code of that global and
getting out the inlining potential. This relies on
lazy-evaluation to avoid running the whole pass.

``` haskell
M b -> reapply params $ RES r (me <&> \e -> e{marked=True})
         where RES r me = inline d env [] b
```

If we see an inline annotation, then we we apply the inliner to
the expression (with no pending arguments) and use that to get
the expansion and the Pot, and then we fill the "inline-mark" on
that Pot.

``` haskell
L v b -> RES (L vr.out br.out) Nothing
           where vr      = inline (d+1) (Nothing  : env) []     v
                 br      = inline (d+1) (noCyc vr : env) params b
                 noCyc x = guard (not $ hasRefTo 0 x.out) >> x.pot
```

For let bindings, the result has not Pot, we just perform the
expansion on the binding expression and on the body expression.

-   When performing expansion on the variable, we set the Pot for
    self-reference to Nothing, and we run the expansion with no
    pending arguments (because arguments are not passed to the
    binding, only to the body).

-   When performing the expansion of the body, we pass in all of
    the pending argument and we take the Pot and set it in the
    environment. HOWEVER, if the binding contains a
    self-reference, then we set the Pot for the binding to
    `Nothing`, otherwise we risk an infinite loop.

``` haskell
A f x -> inline d env (ARG d x' : params) f
           where x' = (inline d env [] x).out
```

For function application, we just run the pass on the argument
and then recurse into the head with the argument added to the
"pending arguments"

``` haskell
F lam -> reapply params (RES out pot)
           where
         need = fromIntegral lam.args
         env' = replicate (1 + need) Nothing <> env
         d'   = 1 + need + d
         out  = F lam{ body = (inline d' env' [] lam.body).out }
         pot  = do guard (not $ hasRefTo lam.args lam.body)
                   Just POT { lam, marked=lam.inline, deep=d, need, args=[] }
```

The first thing we do when doing inline expansion on a function,
is to run the pass on the function body. Then, we just create a
fresh Pot from the lambda.

There are a couple of considerations here:

-   If the function is recursive (contains any self-reference)
    then there is no Pot (otherwise, we risk an infinite loop).

-   If the function is marked "always inline" then we pre-set
    `(.marked)` on the resulting Pot.

### The Inlining Pass: Performing Expansion

> TODO: explain `reapply`.

### Performing a Single Inline Application

> TODO: This code is spliced in from another document, rework this to make
> sense with the above context.

Take the example:

    (const x y)=x

    @ x  (add 3)
    @ kx (**const x)
    | kx (add x)

When it's time to actually do this inline application, we have
the following data:

    currentDepth:Nat
    fn:Lam
    fnDepth:Nat
    args: List (depth:Nat, expr:Sire)

Specifically:

    currentDepth=3
    fn=[pin=1 inline=0 tag={const} args={2} body=1]
    fnDepth=2
    args[0] = [ depth=2 expr=1 ]
    args[1] = [ depth=3 expr=[%A [%V add] 1] ]

And to inline, we want to produce:

    @ x (add 3)
    @ kx (**const x)
    @ a0 0
    @ a1 x
    @ a2 (add x)
    | a1           ;; <- just fn.body

Note that we just splice CONST_BODY in (almost) verbatim. This is
because the bindings we put before it mirror the structure of its
arguments. And that's why we have this nonsense `a0` binding.
Since we know the function isn't recursive, that's just a dummy
binding to fill the "self-reference" slot.

But note that all of the arguments (and free variables within
`fn.body` if it was defined as a local binding) are now moved
into a *deeper* nesting context, so all of the de-bruijn
references need to be renumbered.

The result is:

    , %L [%A [%G add] [%K 3]]
    , %L [%A [%M [%G const]] [%K 1]]]
    , %L [%K 0]
    , %L [%V 3]   ;; note that this args[o].expr=1 before renum
    , %L [%A [%V add] 4]
    , V 1

This renumbering of references (that have been moved to a new
context) is basically the only fiddly part here, the basic idea
is trivial.

## Code Generation

The "code generation" part does four things:

1)  "Ingest" a `Sire` expression into a new representation. While
    doing that, we eliminating constant bindings, ignore trivial
    re-bindings, and fold constant-partial-applications

2)  Calculate a reference-count for each binding, and produce a
    list of used bindings in traversal order.

3)  Use the above information to produce a free list, and do
    lambda-lifting to eliminated nested functions.

4)  Generate PLAN code for the function, using the
    reference-count to inline single-reference bindings (and
    ignore unused bindings).

These four steps are not separate passes, they are all
intermixed.

They *need* to be intermixed. For example, when we do
lambda-lifting we sometimes create new constant values, and we
need to know that during ingestion, so that we can do
constant-propagation. This will make more sense later, once we
get into the code.

### CodeGen Types

``` haskell
data Exp
    = VAL Fan
    | VAR Int
    | APP Exp Exp

data Fun = FUN
    { pin :: Bool
    , tag :: Nat
    , slf :: Int
    , arg :: [Int]
    , bin :: (IntMap Exp)
    , bod :: Exp
    }
```

Here, we move away from the De Bruijn Indices, and switch to an
internal representation that assigns a unique key to each
binding. We also eliminate a bunch of detail. We don't care about
inline markers or the distinction between constant values and
global-variable references.

In a `Fun`:

1)  `pin`, and `tag` have their usual meanings (should the result
    be wrapped in a pin, and what should be the name assigned to
    the resulting law)

2)  `slf` is the unique key for self-reference, and `arg` is one
    unique key per argument.

3)  `bin` is table of all of the let-bindings within a law, where
    the keys of the table are unique binding keys. `bod` is the
    body of the law, which is within the scope of all of the
    let-bindings from `bin`.

And expressions are trivial. They are either constant PLAN
values, variable references (by key), or function application.

So, this input, for example:

    = (f x y)
    @ xy (x y)
    @ x4 (x 4)
    | x xy x4

Would be a `Fun` something like:

``` haskell
FUN
    { pin = True
    , tag = "f"
    , slf = 4
    , arg = [5, 6]
    , bin = mapFromList
        [ (7, APP (VAR 5) (VAR 6))
        , (8, APP (VAR 5) (VAL 4))
        ]
    , bod = (VAR 5 `APP` VAR 7 `VAR` 8)
    }
```

### CodeGen In Detail: Ingestion

``` haskell
ingest :: Sire -> (Exp, (IntMap Exp, Int))
```

Ingest takes a Sire expression and produces an `Exp` and a table
of all of the bindings found within the expression. The `Int` in
the return value is the "unique supply" (the number of unique
binding keys that have been generated).

Here's the whole thing:

``` haskell
ingest :: Sire -> (Exp, (IntMap Exp, Int))
ingest = \top -> runState (go [] top) (mempty, 0)
  where
    gensym = do { (s,v) <- get; put (s,v+1); pure v }

    go s = \case
        V i   -> pure $ (s !! fromIntegral i)
        M x   -> go s x
        G g   -> pure $ VAL g.d.value
        K x   -> pure $ VAL x
        A f x -> ((,) <$> go s f <*> go s x) <&> \case
            (VAL fv, VAL xv) | trueArity fv > 1 -> VAL (fv %% xv)
            (fr, xr)                            -> APP fr xr

        L v b -> do
            k  <- gensym
            vr <- go (VAR k : s) v
            let keep = case vr of { VAR w -> w==k; APP{} -> True; _ -> False }
            when keep do modify' (over _1 $ insertMap k vr)
            go ((if keep then VAR k else vr) : s) b

        F fn@LAM{tag,pin} -> do
            slf      <- gensym
            arg      <- replicateM (fromIntegral fn.args) gensym
            (_, nex) <- get
            let env             = (VAR <$> reverse arg) <> [VAR slf] <> s
            let (bod,(bin,key)) = runState (go env fn.body) (mempty, nex)
            let (cns,free)      = compile key FUN{tag,pin,bod,slf,arg,bin}
            pure $ foldl' APP (VAL cns) (VAR <$> free)
```

``` haskell
ingest :: Sire -> (Exp, (IntMap Exp, Int))
ingest = \top -> runState (go [] top) (mempty, 0)
  where
    gensym = do { (s,v) <- get; put (s,v+1); pure v }

    go :: [Int] -> Sire -> State (IntMap Exp, Int) Exp
    go s = \case
```

We do this in a state monad where the state is the table of
bindings that we've seen, and the unique supply of binding keys.
When we want to generate a unique binding key, we just return the
"next key" and increment that key in the state.

The `[Exp]` is the current scope. Because `Sire` uses-debruijn
indices, we can represent the scope at any point as a list, and
simply index in that list to resolve variables.

These `Exp` values are usually just `(VAR k)`, mapping a
de-bruijn index to a unique binding key. However, if a binding is
just a constant value or a trivial re-binding of another binding,
then we inline it into the call-site. In that case, we store the
whole expression in this environment, not just the new variable
number.

``` haskell
V i   -> pure $ (s !! fromIntegral i)
```

If we see a variable, return the associated expression from the
environment, as discussed above.

``` haskell
M x   -> go s x
```

Ignore inline markers (inlining has already happened before we
run code generation).

``` haskell
G g   -> pure $ VAL g.d.value
K x   -> pure $ VAL x
```

We don't care about the difference between globals and constants
at this point, they are all just constants to us.

``` haskell
A f x -> ((,) <$> go s f <*> go s x) <&> \case
    (VAL fv, VAL xv) | trueArity fv > 1 -> VAL (fv %% xv)
    (fr, xr)                            -> APP fr xr
```

This convers to an `APP` node, but simplifies partial
applications of constant functions. For example, the expression
`(APP (VAL add) (VAL 1))` can be simplified to `(VAL (add 1))`.
This reduces code-size and has no effect on semantics.

``` haskell
L v b -> do
    k  <- gensym
    vr <- go (VAR k : s) v
    let keep = case vr of { VAR w -> w==k; APP{} -> True; _ -> False }
    when keep do modify' (over _1 $ insertMap k vr)
    go ((if keep then VAR k else vr) : s) b
```

For lambdas, we generate a new binding key.

We process the binding expression, with self-reference resolving
to a reference to the new binding key.

If the expression is a constant value or a trivial rebinding of
another expression (and *NOT* a trivial self-reference cycle)
then we eliminate the binding by placing the binding expression
directly into the environment.

Otherwise, we add the binding to the table of bindings, and we
process the body with the binding slot resolving to a reference
to this newly-added binding.

``` haskell
F fn@LAM{tag,pin} -> do
    slf      <- gensym
    arg      <- replicateM (fromIntegral fn.args) gensym
```

When we see a nested function, we either compile that into a
constant value (if it has no free variables), or we lambda lift
the function and compile it to a constant applied to it's free
variables. We'll get into that in more detail in the next
section.

First, we generate a unique binding keys for each argument, and
another one for self-reference.

``` haskell
(_, nex) <- get
let env             = (VAR <$> reverse arg) <> [VAR slf] <> s
let (bod,(bin,key)) = runState (go env fn.body) (mempty, nex)
```

Next, we extend the environment with with self-reference and
arguments, and then we recurse into the body.

Bindings bound within this sub-function belong to that
sub-function, not to the current function, so we do the recursion
in a separate `runState`, but maintaining the same unique supply
(since the nested function can see outside bindings).

``` haskell
let (cns,free)      = compile key FUN{tag,pin,bod,slf,arg,bin}
pure $ foldl' APP (VAL cns) (VAR <$> free)
```

Then, we compile the ingested sub-function to a constant PLAN
values and a list of free variables. We haven't seen that part
yet, but we'll get to it soon. That process produces a constant
value an a list of lifted free variables (that the functon
expects to be passed in as arguments).

Finally we return the compiled function applied to it's free
variables. Note that we don't fold constants applications here,
all of the applications are to variables, so no constant
applications are possible.

Also note that we don't incorporate the "unique supply" from
processing the sub-function. It's been translated to a raw PLAN
value now, so the binding keys that were used during that process
are not longer relevant to us.

### CodeGen In Detail: Reference Counting

The "referencing counting" routine is used for a number of
different things:

1)  For lambda lifting, we need a list of free variables in some
    sort of simple predictable order.

2)  In order to eliminate unused bindings, we need to know which
    bindings are not referenced from the body.

3)  In order to inline single-use bindings, we need to know which
    bindings are reference exactly once (while only counting this
    from references that are actually used.

We do this by doing an traversal over all of the code, starting
at the body, and collecting:

1)  The set of references that we've seen.

2)  The number of times we encountered each reference.

3)  The order in which we finished processing each reference.

This code doesn't actually produce a free variable list, but it
does produce a list of (used) variable references in a simple,
predictable order. That list can later be filtered to produce an
appropriate list of free variables.

Reference counting is implemented as a simple state monad traversal:

``` haskell
stats :: Fun -> (Set Int, Map Int Int, [Int])
stats pam =
    over _3 reverse $ snd $ runState (go pam.bod) (mempty, mempty, [])
  where
    go :: Exp -> State (Set Int, Map Int Int, [Int]) ()
    go VAL{}     = pure ()
    go (APP f x) = go f >> go x
    go (VAR k)   = do
        alreadySeen <- view _1 <$> get
        unless (k `member` alreadySeen) do
            modify' (over _1 $ S.insert k)
            maybe (pure()) go (lookup k pam.bin)
        modify' \(seen, tab, lis) ->
            let c = fromMaybe 0 (lookup k tab)
            in (seen, (insertMap k (c+1) tab), (if c==0 then k:lis else lis))
```

This is mostly straightforward code, so I'll just note a couple
of things and leave it at that.

1)  The `alreadySeen` stuff is important to avoid inifinine loops
    on self-recursive bindings, and to avoid double-processing a
    binding that is referenced from multiple places.

2)  The `maybe (pure()) go (lookup k pam.bin)` guard handles the
    case where that variable reference points to a free variable,
    self-reference, or argument. In those cases, there is no
    binding to recurse into.

### CodeGen In Detail: Creating PLAN Laws

Let's talk about the `isCodeShaped` utillity first:

``` haskell
isCodeShaped :: Nat -> Fan -> Bool
isCodeShaped maxArg f = case kloList f of
    [NAT n]       -> n <= maxArg
    [NAT 0, _, _] -> True
    [NAT 1, _, _] -> True
    [NAT 2, _]    -> True
    _             -> False
```

In PLAN laws, if an expression tree doesn't match any of the
built-in patterns, it's treated as a constant value.  For example:

    | 0 "foo" 2 (0 1 2)  ;;  means (foo x y ? x y)

    | 0 "foo" 2 (0 1)    ;;  means (foo x y ? (0 1))

We take advantage of this behavior to reduce code size and make
code more readable. If we are generating a constant value, and it
is "code-shaped" then we need to wrap it with `(2 _)`, otherwise
we can just inline it directly.

Here's an example of the difference this makes:

    (f x) ? (add x (add 2 3))

    ;; Without the optimization:
    (0 {f} 1 (0 (0 (2 add) 1) (0 (2 (add 2)) (2 3))))

    ;; With the optimization:
    (0 {f} 1 (0 (0 add 1) (0 (add 2) 3)))

Alright, here's the process for actually creating the PLAN laws.
Note that this handles eliminating zero-use and single-use
bindings by simply not emiting code for them. And we inline
single-use bindings into the call-site by simply recursing into
the code for the binding instead of emiting a reference to a
binding.

Okay, let's get into it:

``` haskell
codeGen :: Fun -> (Set Int, Map Int Int, [Int]) -> Fan
codeGen fn (_, refcounts, refSeq) =
    (if fn.pin then (4 %%) else id)
    (0 %% NAT fn.tag %% NAT numArgs %% foldr bind (gen fn.bod) binds)
  where
    look k v      = fromMaybe (error "impossible") (lookup k v)
    keep k        = fromMaybe 0 (lookup k fn.bin >> lookup k refcounts) > 1
    binds         = filter keep refSeq
    numBinds      = fromIntegral (length binds)
    numArgs       = fromIntegral (length fn.arg)
    maxRef        = numArgs + numBinds
    scope         = fn.slf : (fn.arg <> binds)
    table         = mapFromList (zip scope [0..]) :: Map Int Nat
    bind k rest   = 1 %% gen (look k fn.bin) %% rest
    gen (VAL k)   = if isCodeShaped maxRef k then (2 %% k) else k
    gen (APP f x) = 0 %% gen f %% gen x
    gen (VAR v)   = case (lookup v refcounts, lookup v fn.bin) of
                        (Just 1, Just bx) -> gen bx
                        _                 -> NAT (look v table)
```

> TODO: Explain this code

### CodeGen In Detail: Compiling Laws to PLAN

The `compile` function takes a unique supply and an
already-ingested `Fun` compiles it into a PLAN value, and also a
list of free variables that the resulting law expects to be
passed.

Before we get into the code, let's briefly go over what lambda
lifting is.  Let's examine the following example:

    & y
    ? (f x)
    | add x y

Let's say that we want to lift the `f` function. This can't
immediatly be compiled into a PLAN law, because it references
`y`, which is a "free variable", it's bound outside of `f`. Note
that `add` is not a free variable, since it's a known
globally-bound value, and we treat that as an embedded constant
value.

We solve this will a process called "lambda lifting", where we
add all of the free variables to the front of the argument list,
and then pass in.

    & y
    |   ? (f y x)
        | add x y
    | y

After this transformation, `f` has no free variables, and can be
translated directly to a PLAN law.  There is one complication, however.
Look at this example:

    & y
    ? (f x)
    | if x y (f 1)

Here the `f` function is recursive, so we need to remember to
pass the extra arguments whenever we self-reference, also.

    & y
    |   ? (f y x)
        @ recur (f y)
        | if x y (recur 1)
    | y

Here's the whole `compile` routine:

``` haskell
compile :: Int -> Fun -> (Fan, [Int])
compile nex f1 = (codeGen f3 stat3, free)
  where
    stat1@(_, _, !refs1) = stats f1
    free                 = filter (isFree f1) refs1
    (f3, stat3)          = if null free then (f1, stat1) else (f2, stats f2)
    isFree FUN{..} k     = not $ or [k == slf, (k `member` bin), (k `elem` arg)]
    f2 = f1 { slf = nex
            , arg = free <> f1.arg
            , bin = insertMap f1.slf (foldl1 APP $ map VAR (nex : free)) f1.bin
            }
```

This is dense code, so let's walk through it.

``` haskell
stat1@(_, _, !refs1) = stats f1
free                 = filter (isFree f1) refs1
isFree FUN{..} k     = not $ or [k == slf, (k `member` bin), (k `elem` arg)]
```

The first thing we do, is we collect reference counts for the
function we're given, and we use that to construct a free list.
We take the traversal-order list of used-references, and we
filter it using `isFree`.

`isFree` is true for anything that's not bound in this function,
and the things that are bound in this function are: 1) self-reference 2)
let bindings 3) function arguments. So, `isFree` just checks each
reference to see if it's bound in any of those places.

``` haskell
(f3, stat3) = if null free then (f1, stat1) else (f2, stats f2)
```

Now that we have a free variable list, we need to do lambda lifting.
If there are no free variables, we just skip this and use the input
without modification.

If there *are* free variables, we need to re-run the reference-counting
routine, since we've added a new binding that maybe be reference
zero-or-one times.

If there are free variables, we do this:

``` haskell
f2 = f1 { slf = nex
        , arg = free <> f1.arg
        , bin = insertMap f1.slf (foldl1 APP $ map VAR (nex : free)) f1.bin
        }
```

`slf=nex` replaces the key for self-reference with a fresh key,
and we will add a new binding under the old key to do the `recur`
stuff mentioned above.

`bin = insertMap f1.slf (foldl1 APP $ map VAR (nex : free)) f1.bin`
creates the new self-reference binding.

`arg = free <> f1.arg` just adds the free variables to the front of the
argument list.

``` haskell
compile nex f1 = (codeGen f3 stat3, free)
```

And finally, we do the actual conversion to a PLAN law using `codeGen`.


### CodeGen In Detail: Compiling Top-Level Sire Expressions

Finally, our `compile` routine works in terms of functions, but
the top-level expressions that we want to compiler are arbitrary
`Sire` expressions, not just functions. Consider this input:

    @ x 3
    @ y 4
    ^ map (add x) [1 2 3]

The way we resolve this is just by wrapping that in a dummy
function, and then calling it.

    |   & _
        @ x 3
        @ y 4
        ^ map (add x) [1 2 3]
    | 0

Here's the code that ties everything together:

``` haskell
compileSire :: Sire -> Fan
compileSire raw = fst (compile (n+2) fun) %% 0
  where (bod, (bin, n)) = ingest (inline 0 [] [] raw).out
        fun = FUN{pin=False, tag=0, slf=n, arg=[n+1], bin, bod}
```

Note that we ignore the "free variable list" from `compile`,
because we know that this is a top-level expression and therefore
cannot have any free variables.
