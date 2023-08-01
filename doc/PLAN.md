# PLAN

PLAN is an evaluation model for a purely-functional database.

Actually, this "purely-functional database" phrase is a bit misleading,
it's an attempt to describe a novel programming paradigm for which we
don't yet have good vocabulary.

Urbit, another model in the same paradigm, describes itself as a "purely
functional operating system".  You could also describe the paradigm
by saying that it is a way to write "permanent programs" that survive
hardware restarts without losing state.  All of these descriptions amount
to essentially the same thing, and yet I suspect that the reader still
does not understand what is being described.

In the traditional computing paradigm, there is a strict separation
between memory and disk.  Programs load data from disk, do things
with that data, and then store new data by writing new data to disk.
If the machine running that program shuts down, all of the data in memory
goes away.

In the traditional paradigm, different data models are used for data that
lives on disk and for data that only lives in memory.  The layout of
data in memory is mostly arbitrary, corresponding only to the specific
needs of one version of one program.  On the other hand, the layout of
data on disk needs to be much more stable.  A basic SQL table from two
decades ago still makes sense today.  A directory full of MP3 files,
stored 10 years ago in an NTFS file system, still makes sense today.

The "purely functional database" paradigm *collapses the distinction*
between memory and disk.  The state of a purely functional database is a
function value which implements the entire database logic.  Updates are
performed by passing an argument to this function and using the result
as the new state.  This is represented on disk as a snapshot and a log
of inputs since that snapshot.

This makes persistence to disk automatic and transparent.  Implementing
this effectively introduces a bunch of novel constraints on the evaluation
model.  In particular:

-   The "in memory" representation of data needs to be *stable*.
    It will also be used on disk, so any breaking change to the data
    model is a breaking change to all stored data.

-   The "in memory" representation of data needs to be *portable*.
    It will be used on disk, and the on-disk representation should not
    be too closely tied to a specific implementation.

-   All "in memory" data will also be data on disk, and the on-disk
    layout needs to be efficient.  Good data locality on disk, load and
    save data very efficiently, etc.

-   Since functions are "just data" in a functional language, the
    representation of functions must also be stable, portable, and have
    an efficient on-disk representation.

-   Functions must be able to define new functions.

    Since databases are represented as functions, and all updates are
    done by passing arguments to that function, the only way to change
    the function itself, is if the function can create new code based
    on the data passed in.

One major consequence of that last point, is that functions must be
"plain old data".  They must be normal, inspectable objects, and their
representation cannot contain implementation details.  In almost all
languages, functions are opaque data types, but the constraints of this
paradigm makes that impossible.


## A Tour

PLAN has a very simple data model.  "PLAN" is an acronym, each letter
corresponds to one type of data:

-   Pins: A pin `<x>` is a simple box containing the single value
    `x`.  This is used to control data layout in memory and on disk and
    makes full-normalization cheap.

-   Laws: A law `{n a b}` is a user-defined function named `n` that takes
    `a` arguments, and has a body `b`.

-   Apps: `(f x)` is a partially applied function.  This is only possible if
    `f` has an arity greater than one.

-   Nats: Natural numbers of arbitrary size.


### Laws

Laws are user defined functions.

Laws are a very simple type of function.  Laws accept a fixed number of
arguments and return a single value.

Laws can't contain nested laws, but can reference other laws as constant
values.  Functions with nested laws can be compiled into laws using
lambda lifting.

This corresponds to the "supercombinator" concept, which is perfect
for our needs.  In particular:

-   It can be implemented efficiently without any complex transformations.

-   It has a very straight-forward encoding.  Functions with nested
    lambdas can be compiled to laws using lambda lifting.

-   Laws can be constructed directly from normal code, without having
    any sort of special `eval` function built into the runtime system.

The language of laws is something like:

    Law = 
      | self                -- self-reference
      | var x               -- reference bound variable.
      | const k             -- produce constant value
      | (Law Law)           -- function application
      | let x = Law in Law  -- let binding (bindings may self-reference)

For example:

    const x y = x

    constThree x = 3

    fix f = let x = f x in x

Laws bodies are encoded using partially applied primops:

    |---------|--------------------|
    | pattern | meaning            |
    |---------|--------------------|
    | (0 f x) | (f x)              |
    | (1 v b) | let v = b          |
    | (2 k)   | const k            |
    | 0       | self               |
    | 1       | var 1              |
    | 2       | var 2              |
    | x       | const x (fallback) |
    |---------|--------------------|

There are four built-in operations, indicated by the first four natural
numbers.

-   `(0 n a b)`: Construct a new law: `{n a b}`

-   `(1 p l a n x)`: Pattern match on the value `x` (is it a pin, a law,
    an app, or a nat?).

        case x of
            PIN <i>     -> p i
            LAW {n,a,b} -> l n a b
            APP (f x)   -> a f x
            NAT x       -> n x

-   `(2 z p x)`: Pattern match on a natural number (is it zero or (n+1)?).

        case x of
            0 -> z
            x -> p (x-1)

-   `(3 x)`: Increment a natural number.

-   `(4 x)`: Normalize x and construct a new pin `<x>`.

PLAN evaluation is lazy, but each state-machine transition does a full
normalization, so long-lived space leaks are impossible, and the on-disk
representation will never contain unevaluated thunks.


## Pins

The "pins" concept formally is just a box that contains a value that is
guaranteed to be in normal form.

There are some additional guarantees that pins are supposed to adhere to
in practice:

-   Comparing two pins for equality should be a constant-time operation.

-   Pins are globally de-duplicated before being written to disk.

-   Pins that have been persisted to disk should be stored in a contiguous
    block.  The only pointers "out" of the block should be pointers to
    other pins.

This is similar to the concept of "interning" strings.  You get constant
time equality checks and data need only be stored once.

Pins are very similar to the GHC concept of "Compact Regions", except
that each region is immutable, and regions can reference other regions,
forming a DAG.

Pins solve the following problems:

-   Pins give users control over data layout.

-   Pins make full-database normalization tractable.

    Database state transitions need to normalize the entire state, but
    normalization requires a full traversal over the entire database.
    Pins short-circuit this process, since a pin is always in normal form.

-   Pins make garbage-collection fast, even with massive heaps.

    Garbage collection requires a full heap-traversal, which starts to
    be a problem with extremely large heaps.  Pins mitigate this problem
    because (for long-lived pins) pins, pins never contain reference
    inside another pin.

    Because of this, a full garbage collection pass only needs to traverse
    the pins DAG.  It doesn't need to look at the data inside the pins.


## Jets

You may notice that the language contains very few operations, and doing
everything with just increment/decrement is not very practical.

In practice, runtime systems need to implement more operations natively.
Multiplication of natural numbers, for example, is prohibitively expensive
if implemented directly on top of the PLAN primitives.

To solve this, PLAN adopts the "Jets" concept from Urbit.

Jets work by choosing a specific function, and making that "special".
This one, for example, is special:

    toNat=(4 (0 499848736628 1 (0 (2 0 3) 1)))
    toNat=<{499848736628 1 (0 (2 0 3) 1)}>

When the runtime runs a special function, instead of running the legal
code as usual, it runs a special built-in routine.  The optimized version
needs to have exactly the same behavior as the unoptimized version.
This is is similar to the concept of an "Intrinsic Function"

At first brush, this may seem like an over-complicated and
roundabout way to add new primitives.  But remember that the
perfect-forward-compatibility guarantee means that we can never change the
shape of data.  And also remember that functions are not opaque objects.
So these details cannot be hidden away, adding a new primitive in the
normal way, would be a breaking change that requires all data be migrated
to a new format.

In theory, this system creates perfect compatibility between
implementations.  If an implementation doesn't know about a special
function, then it will still arrive at the same answer.

However, in practice, an extremely slow programs is not so different
from a program that doesn't work at all.  So, changing or removing a
special function is, in practice, a breaking change.  We solve this by
just never doing that.  The set of special functions is specified in
another document.

Furthermore, *adding* new special functions can sometimes be a
backward-compatibility breaking change.  Specifically, if we add a new
special function, and then write code that depends on that function
being fast, then the resulting database will run very slowly on old
implementations.


## A Specification

This section includes a complete specification for the PLAN system.
The spec written as a naive, but complete implementation in pseudo-code.

Some notes on notation:

-   `(f x y)` is a short-hand for `((f x) y)`.

-   `x:@` `y:(f x)` is pattern-matching syntax: "x is a natural number",
    "y is a pair of f and x".

-   The `:=` operator creates a new local binding.

-   The `<-` operator overwrites an existing object in-place.

-   Black holes are only used during evaluation.  A black hole is
    a placeholder value for something that has not yet been evaluated.

-   If a series of pattern matches does not handle a pattern, the result
    is a crash.  For example `E(x)` crashes if x is a black hole.


```
Every PLAN vaue is either a pin x:<i>, a law x:{n a b}, an app x:(f g), a
nat x:@, or a black hole x:<>.  Black holes only exist during evaluation.

Run F(x) to normalize a value.

E(o:@)     = o                          | F(o) =
E(o:<x>)   = o                          |     E(o)
E(o:(f x)) =                            |     when o:(f x)
    E(f)                                |          F(f); F(x)
    when A(f)=1                         |     o
        o <- X(o,o)                     |
        E(o)                            | N(o) = E(o); if o:@ then o else 0
    o                                   |
E(o:{n a b}) =                          | I(f, (e x), 0) = x
    if a!=0 then o else                 | I(f, e,     0) = e
        o <- <>                         | I(f, (e x), n) = I(f, e, n-1)
        o <- R(0,o,b)                   | I(f, e,     n) = f
        E(o)                            |
                                        | A((f x))     = A(f)-1
X((f x), e)         = X(f,e)            | A(<p>)       = A(p)
X(<p>, e)           = X(p,e)            | A({n a b})   = a
X({n a b}, e)       = R(a,e,b)          | A(n:@)       = I(1, (3 5 3), n)
X(0, (_ n a b))     = {N(n) N(a) F(b)}  |
X(1, (_ p l a n x)) = P(p,l,a,n,E(x))   | R(n,e,b:@) | b≤n = I(x,e,(n-b))
X(2, (_ z p x))     = C(z,p,N(x))       | R(n,e,(0 f x))   = (R(n,e,f) R(n,e,x))
X(3, (_ x))         = N(x)+1            | R(n,e,(1 v b))   = L(n,e,v,b)
X(4, (_ x))         = <F(x)>            | R(n,e,(2 x))     = x
                                        | R(n,e,x)         = x
C(z,p,n) = if n=0 then z else (p (n-1)) |
                                        | L(n,e,v,b) =
P(p,l,a,n,(f x))   = (a f x)            |     x := <>
P(p,l,a,n,<x>)     = (p x)              |     f := (e x)
P(p,l,a,n,{n a b}) = (l n a b)          |     x <- R(n+1,f,v)
P(p,l,a,n,x:@}     = (n x)              |     R(n+1,f,b)
```
