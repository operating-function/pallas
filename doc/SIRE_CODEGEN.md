# Compiling Sire to PLAN

This document explains the overall idea architecture of the Sire
code generator; what it's trying to do. If you want a detailed
explanation of *exactly what* it does, see
[SIRE_BACKEND.md](SIRE_BACKEND.md).

Once the front-end has finished macro-expansion, parsing, name-resolution,
and end up with something very close to PLAN.

``` haskell
data Sire
    = V Nat        --  Variable reference
    | K Fan        --  Constant value
    | G Binding    --  Globally-bound constant value
    | A Sire Sire  --  Function application
    | L Sire Sire  --  Let-binding
    | M Sire       --  Inline annotation
    | F Lam        --  Nested lambda
```

Essentially, this is just PLAN plus nested lambdas. If we just
lambda-lift all of the nested functions, then we will get
something that can be directly translated to PLAN.

However, the resulting code will be ugly for a number of reasons:

1)  Macro-expansion, inlining, and lambda lifting all generate new
    bindings.  And a lot of these bindings are useless:

    a)  Unused bindings.

    b)  Single-use bindings.

    c)  Bindings of constant values.

    d)  Trivial re-bindings of other variables.

    This is especially significant, because some of these bindings
    result in unneccessary arguments when we do lambda-lifting.

2)  There are a couple of code-size reduction optimizations that
    are too annoying to do by hand, that improve the legibility
    of the result, and that have no effect on behavior.

    a)  Constants can sometimes omit the (2 x) wrappers.

    b)  Constant partially-applications can just be done up-front.
        `(0 (2 add) (2 1))` means the same thing as `(add 1)`.

So, we want to perform these optimizations as we do
code-generation.

Note that these are all simple, predictable optimizations, and
they all exist to generate more readable, smaller code. We want
the PLAN code for jets to be *pristine* and it's much easier to
do that with these optimizations.

Again, this is *NOT* about performance, only code beauty. From a
performance perspective, all of these transformations could just
as easily be done by the runtime system.
