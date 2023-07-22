# PLAN

PLAN is a hyper-minimalistic purely functional programming system with
a frozen specification.

It is somewhat analogous to the JVM, except that the model is purely
functional with no escape hatches (no unsafePerformIO, no FFI, etc).  PLAN
evaluation is always fully deterministic and referentially transparent.

Every PLAN value is represented using a simple data model, and PLAN code
can pick apart and reconstruct any PLAN value, including functions.

The goal of PLAN is to be a system for implement "persistent state
machines", where the state of the machine is kept synced to disk, and
runtimes system can evolve without breaking old state machines.

These state machines:

-   can include their own toolchain and upgrade themselves, by dynamically
    compiling new PLAN functions.

-   can serialize themselves and send copies of themselves across the
    network.


## Specification

This section includes a complete specification for the PLAN system.
The spec written by writing a naive implementation in pseudo code.

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
Every PLAN vaue is either a pin x:<i>, a law x:{n a b}, an app x:(f g),
a nat x:@, or a black hole x:<>.

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


## Universal Pattern Matching

Using the 1 and 2 primop, every PLAN value can be picked apart with
pattern matching.  And using the `0` and `4` primops, any value can
be reconstructed.

For example, using the above rules, we can write code to implement
the functions:

    save : Any -> ByteString
    load : ByteString -> Maybe Any

Such that:

    for all x, (JUST x == load (save x))


## Pins

The "pins" concept formally is just a box that contains a value that is
guaranteed to be in normal form.

There are some additional guarantees that pins are supposed to adhere to
in practice:

-   Comparing two pins for equality should be a constant-time operation.

-   Long-lived pins should be globally de-duplicated.

-   Long-lived pins should be stored in contiguous memory, the only
    out-pointers should be to other pins.

-   If a large PLAN value is written to disk, pins should be de-duplicated
    first.

This is similar to the concept of "interning" strings.  You get constant
time equality checks and data need only be stored once.

Pins are very similar to the GHC concept of "Compact Regions", except
that each region is immutable, and regions can reference other regions,
forming a DAG.


## Jets

The PLAN runtime implements a number of extra primops, like `toNat`,
`add`, `div`, `eql`, etc.

These primops are formally implemented with a value such as:

    toNat=(4 (0 499848736628 1 (0 (2 0 3) 1)))

    (Note that 499848736628 is an ASCII representation of the string
    "toNat" encoded to a number using the least-significant-first
    byte order)

In other words, the "formal value" of a primop is a specific piece of
PLAN code that implements a primop.  Adding a new primop can change the
performance characteristic of code, but the formal behavior must always
match the spec.

When the runtime constructs this pin, it recognizes it as a primop
(using the same machinery that is used for deduplication).  Then,
instead of generating code for the pin using the pin, we use special
routines hard-coded into the runtime system.

This architecture makes it possible to add new primitives without
modifying the formal semantics or modifying the shape of existing code.
In other words, adding new primops does not break existing persistant
state machines.

The set of jets is standardized using Sire, which is discussed elsewhere.
