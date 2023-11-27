# Inlining in Sire

Sire supports inlining, but in a deterministic way with
predicable results. Inlining is always something the user
explicitly opts-into and not something that compiler does
heuristically as an optimization.

Essentially, inlining in sire is just another kind of "macro", a
way to factor out certain coding patterns without making the
resulting code indirect and inefficient.

The process of inlining in Sire is mostly straightforward, but
there are some corner cases that need to be handled in exactly
the same way in every implementation. This document is somewhat
long because it lays out the whole process in excruciating
detail and explains why things work they way that they do.

## Basic Inlining

Essentially, the rule is just:

> Given an expression `(**f x y z)`, if `x` is a reference 1) to
> a known function, 2) that is not recursive, 3) that takes 3 (or
> fewer) arguments, then we perform inline expansion.
>
> Here, a "know function" is either:
>
> 1.  A globally bound function expression.
> 2.  A locally bound function expression.
> 3.  A lambda expression.

Inline expansion works by simply creating a let-binding for each
argument-expression and then splicing in the function body
verbatim. For example, if `f` is the function
`(f a b c ? c a b a)`, then the result of inline-expansion on
`(**f x y z)` is:

    @ a x
    @ b y
    @ c y
    | c a b a

## "Always-Inline" Functions

Actually, even un-annotated expressions (f x y z) are inlined if
`f` is a "known function" that has requested to always be
inlined:

    (**ifNot cond else then)=(if cond then else)
    (ifNot TRUE (error "lol") "ok")

        ==>

    @ cond TRUE
    @ else (error "lol")
    @ then "ok"
    (if cond then else)

## Inlining Through Let Bindings

Inlining also works through let bindings.

    @ ex (**ifNot TRUE)
    | ex (error "lol") "ok"

    @ ex (**ifNot TRUE)
    @ cond TRUE
    @ else (error "lol")
    @ then "ok"
    (if cond then else)

## Inlining Multiple Times

Inlining can produce expressions that contain further
opportunities for inlining. For example:

    = (maybeCase opt non som)
    | if isNat-opt non
    | **som cdr-opt

Here, the `som` callback is not inlined because it is not a
"known function". However, if we inline a call to this function:

    | **maybeCase (SOME 9) (die 'expected SOME')
    & someValue
    | add 3 someValue

The first inline step results in:

    @ opt (SOME 9)
    @ non (die 'expeected SOME')
    @ som (someValue & add 3 someValue)
    | if isNat-opt non
    | **som cdr-opt

And now, `**som` does indeed point to a known function. This
allows us to inline again.

    @ opt (SOME 9)
    @ non (die 'expeected SOME')
    @ som (someValue & add 3 someValue)
    | if isNat-opt non
    @ someValue cdr-opt
    | add 3 someValue

Then, if we eliminate useless let-bindings, we this final result.
Clean and efficient:

    @ opt (SOME 9)
    | if isNat-opt (die 'expeected SOME')
    | add 3 cdr-opt

This process is mostly straight-forward "after you perform an
inline expansion, also apply the inlining pass to the result".

## A Specification

All of this is pretty simple, but there are a number of
edge-cases that need to be handled carefully. Sire is designed to
be deterministic, minimal, and fully-specified so divergences
between implementations are unacceptable. In particular, this
system is used to define jets, so an implementation that doesn't
exactly implement the spec is totally broken implementation.

### Code Representation

This section goes into detail about exactly how inline-expansion
is performed.

In Sire, every global binding stores the source code used in the
binding. The source code that is stored has already been
macro-expanded, parsed, and all names have been resolved.

However, the stored expression is just the input AST, it has not
undergone inlining, lambda lifting, let-optimization, nor
code-generation.

These Sire expressions are stored as a simple syntax tree using
de-bruin indices for variable reference.

    type Lam = { pin:Bool, inline:Bool, tag:Nat, arity:Nat, body:Sire }

    data Sire
        = VAR Nat
        | VAL Any
        | GLO Binding
        | APP Sire Sire
        | LET Sire Sire
        | LIN Sire
        | LAM Lam

A `Lam` lambdas is represented as an row:

    [Bit Bit Nat Nat Sire]

And a `Sire` AST is represented as a PLAN values with one of the
following forms:

    @                    ; S_VAR
    [%val Any]           ; S_VAL
    [%ref (Pin Binding)] ; S_GLO
    [%app Sire Sire]     ; S_APP
    [%let Sire Sire]     ; S_LET
    [%lin Sire]          ; S_LIN
    [%lam Lam]           ; S_LAM

A `Binding` is an array:

    [key:Nat value:Any code:Sire location:Any name:Any]

`key` is a unique number for the binding, `value` is the actual
value bound, `code` is the code to bind (this is what we use for
inlining), `location` is the module this was defined within, and
`name` is the name that this binding was ties to within that
module.

### Finding Opportunities to Inline

:TODO: Explain this for actually. This is a very rough sketch.

Basically we just walk the expression and accumulate from two
directions at once.

Take the example:

    (const x y)=x

    @ x  (add 3)
    @ kx (**const x)
    | kx (add x)

We always track whether the result of an processing an expression
is something that could potentially contribute to inlining.

-   `const` might, because it could be wrapped by a `**`.

-   `(const 3)` can't, because a `**` wrapper needs to be on the
    function itself, so this can never be inlined.

-   `(**const 3)` might, because the function was marked. But we
    still need one more argument before we can do anything.

Whenever there's a binder we store the "inline potential" for it,
so as we walk through the example:

    @ x  (add 3)
    @ kx (**const x)
    | kx (add x)

    - add      (might be inlined)
    - (add 3)  (can't be inlined)

    - REMEMBER: @ x NONE

    - const        (might be inlined)
    - **const      (might be inlined)
    - (**const x)  (might be inlined)

    - REMBMER: @ kx (SOME (**const x))

    - kx            (might be inlined)
    - add           (might be inlined)
    - (add x)       (can't be inlined)
    - (kx (add x))  (can be inlined, and given all arguments)

That illustrates the "left-side tracking, the tracking of the
inlinability of the head-expressions (and the inilinablity of
each binder)

For the "right-side tracking of applications", this example is
better:

    | @ k **const
      | k 4
    | 3

When we hit this outside %app, we don't know if the head is ready
for processing, so we process the argument first and then
remember it:

    SEE: | _ 3
    REMBMER arg[0]=3
    SEE: @ k **const
    REMEMBER @ k (SOME **const)
    SEE: (k 4)

Then at the end, we have an inlinable thing:

    SOME (**const 4)

But we've also remembered an argument, so we re-apply it here:

    SOME (**const 3 4)

Which gives us something that we can expand.

This right-side tracking is equivalent to just moving lets
outside of apps:

    | @ k **const
      | k 4
    | 3

    ==>

    @ k **const
    | | k 4
    | 3

But it's easier to implement using a single pass.

### Performing a Single Inline Application

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
    args[1] = [ depth=3 expr=[%app [%ref add] 1] ]

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

    , %let [%app [%ref add] [%val 3]]
    , %let [%app [%lin [%ref const] 1]]
    , %let [%val 0]
    , %let 3                     ;; note that this args[o].expr=1 before renum
    , %let [%app [%ref add] 4]
    , 1

This renumbering of references (that have been moved to a new
context) is basically the only fiddly part here, the basic idea
is trivial.
