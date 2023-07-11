# PLAN

PLAN is a purely function programming system without escape hatches (no,
unsafePerformIO, FFI, etc), as a consequence PLAN is fully referentially
transparent.

This is important for Plunder, because we want it to be possible to move
a running program from one runtime system to another without any change
of behavior.

Plunder also has a unique feature of having all values (including
functions) be introspectable.  This is necessary in order to implement
Plunder's persistence model.


## Universal Pattern Matching

Every PLAN value can be pattern matched on, including functions.

It is possible to write the following functions:

    serialize : Any -> ByteString
    deserialize : ByteString -> Maybe Any

Such that:

    for all x, (Just x)==(deserialize (serialize x))

These functions can be implemented using the `0` and `1` primops.


## Pins

PLAN has a concept of a "pin", which is formally just a box containing
a single value.

These magic boxes are used to break up large values into pieces that
can be stored separately on disk.

Pins are also globally deduplicated in memory.

This is similar to the concept of "interning" strings.  You get constant
time equality checks and data need only be stored once.  Pins are the
building blocks of Plunder's persistence model.


## Jets

The PLAN runtime implements a number of extra primops, like `toNat`,
`add`, `div`, `eql`, etc.

These primops are formally implemented with a value such as:

    toNat=(4 (0 'toNat' 1 (0 (2 0 3) 1)))

When the runtime constructs this pin, it recognizes it as one of these
"extra primops" and specializes it's execution path.

This architecture makes it possible to add new primitives without
modifying the formal semantics or modifying the shape of existing code.
As a consequence, old programs do not need to be modified to run on
new implementations, and new programs will continue to run on old
implementations.


## Specification

```
Every PLAN is a pin x:<i>, a law x:{n a b}, an app x:(f g), or a nat x:@.

Evaluate by reducing repeatedly with (->).  Reduce outer trees first.

(f g x) := ((f g) x)

NTH(v,n,fb) is the nth element of the array v (or fb if out of bounds)
LEN(v)      is the length of the array v.
PUSH(v,x)   is the array v with x appended.

NAT(x) = if x:@ then x else 0

RUN(r,(0 f x)) = (RUN(r,f) RUN(r,x))
RUN(r,(1 v k)) = RUN(s,k) where s = PUSH(r,RUN(s,v))
RUN(r,(2 v))   = v
RUN(r,v:@)     = NTH(r,v,v)
RUN(r,x)       = x

v:@                          =>  ARITY(v)=NTH([3 4 3], v, 1)
v:(f _)                      =>  ARITY(v)=(ARITY(f)-1)
v:{n _ _}                    =>  ARITY(v)=n
v:<i>                        =>  ARITY(v)=ARITY(i)
ARITY(v)>0                   =>  WHNF(v)
v:@                          =>  NF(v)
v:(f g) WHNF(v) NF(f) NF(g)  =>  NF(v)
v:{n a b}                    =>  NF(v)
WHNF(n) WHNF(t) NF(b)        =>  (0 n a b)     -> {NAT(n) NAT(a) b}
v:<i>                        =>  (1 p _ _ _ v) -> (p i)
v:{n a b}                    =>  (1 _ l _ _ v) -> (l n a b)
v:(f x) WHNF(v)              =>  (1 _ _ a _ v) -> (a f x)
v:@                          =>  (1 _ _ _ n v) -> (n v)
WHNF(v) w=NAT(v)             =>  (2 z p v)     -> if w=0 then z else (p (w-1))
WHNF(x)                      =>  (3 x)         -> NAT(x)+1
NF(x)                        =>  (4 x)         -> <x>
f:@ f>4 NF(x)                =>  (f x)         -> *crash*
f:{n a b} a=LEN([..])        =>  (f ..)        -> RUN([f ..],b)
f:<{n a b}> a=LEN([..])      =>  (f ..)        -> RUN([f ..],b)
f:<i> ARITY(i)=LEN([..])     =>  (f ..)        -> (i ..)
```
