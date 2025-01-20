# Inlining in Sire

This document explains the overall idea archtecture of the Sire
inliner; what it's trying to do. If you want a detailed
explaination of *exactly what* it does, see
[SIRE_BACKEND.md](SIRE_BACKEND.md). The process of inlining in
Sire is mostly straightforward, but note that the behavior is
very particular. All implementations of Sire are expected to
*exactly* match the behavior described in this document.

Sire supports inlining, but in a deterministic way with
predicable results. Inlining is always something the user
explicitly opts-into and not something that compiler does
heuristically as an optimization. Essentially, inlining in sire
is just another kind of "macro", a way to factor out certain
coding patterns without making the resulting code indirect and
inefficient.

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

There is small complication, which is that un-annotated
expressions (f x y z) are inlined if `f` is a "known function"
that has requested to always be inlined:

    (**ifNot cond else then)=(if cond then else)
    (ifNot TRUE (error "lol") "ok")

        ==>

    @ cond TRUE
    @ else (error "lol")
    @ then "ok"
    (if cond then else)

## Inlining Through Let Bindings

The inlining algorithm can *see through* let bindings.

    @ ex (**ifNot TRUE)
    | ex (error "lol") "ok"

    ==>

    @ ex (**ifNot TRUE)
    @ cond TRUE
    @ else (error "lol")
    @ then "ok"
    (if cond then else)

Similarly, more complex patterns of let-bindings are also "seen through":

    | @ k **const
      @ k3 (k 3)
      | k3
    | 4

    ==>

    @ k **const
    @ k3 (k 3)
    @ a 3
    @ b 4
    | a


## Flexible Head Marking

The inline annotation doesn't have to be *exactly* on the lambda, it can
at any point in the application head, except where the marked expression
is over-saturated.

     (const x y)=x

     (**const a b c d)    ==>  (a c d)
     (**(const a) b c d)  ==>  (a c d)
     (**(const a b) c d)  ==>  (a c d)
     (**(const a b c) d)  ==>  ((**const a b c) d)
     (**(const a b c d))  ==>  (**(const a b c d))


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

Then, if we eliminate useless let-bindings, we get this
final result.  Clean and efficient:

    @ opt (SOME 9)
    | if isNat-opt (die 'expeected SOME')
    | add 3 cdr-opt

This process is mostly straight-forward "after you perform an
inline expansion, also apply the inlining pass to the result".
