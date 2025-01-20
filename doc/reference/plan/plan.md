---
id: plan-reference
title: PLAN
---

# PLAN reference

> Reference for implementing a PLAN interpreter

::::warning[TODO]
Forthcoming
::::

Pseudocode specification of PLAN:

```unset
Every PLAN value is either a pin x:<i>, a law x:{n a b}, an app x:(f g),
a nat x:@, or a black hole x:<>.  Black holes only exist during evaluation.

(o <- x) mutates o in place, updating its value to equal x.

Unmatched patterns diverge.

Run F[x] to normalize a value, to "evaluate".

F[o] =                                  | N[o] = E[o]; if o:@ then o else 0
    E[o]                                |
    when o:(f x)                        | C[z,p,n] = if n=0 then z else p (n-1)
         F[f]; F[x]                     |
    o                                   | S[o:(f x y)]       = (S[(f x)] y)
                                        | S[o:(<{n a b}> y)] = o
E[o] =                                  | S[o:(<f> x)]       = S[(f x)]
    when o:(f x)                        | S[o]               = o
        E[f]                            |
        when A[f]=1                     | I[f, (e x), 0] = x
            o <- S[o]                   | I[f, e,     0] = e
            o <- X[o,o]                 | I[f, (e x), n] = I[f, e, n-1]
            E[o]                        | I[f, e,     n] = f
    o                                   |
                                        | A[(f x)]     = A[f]-1
X[(f x), e]         = X[f,e]            | A[<p>]       = A[p]
X[<p>, e]           = X[p,e]            | A[{n a b}]   = a
X[{n a b}, e]       = B[a,a,e,b,b]      | A[n:@]       = I[1, (3 5 3), n]
X[0, (_ n a b)]     = W[N[n],N[a],F[b]] |
X[1, (_ p l a n x)] = P[p,l,a,n,E[x]]   | R[n,e,b:@] | b≤n = I[_,e,(n-b)]
X[2, (_ z p x)]     = C[z,p,N[x]]       | R[n,e,(0 f x)]   = (R[n,e,f] R[n,e,x])
X[3, (_ x)]         = N[x]+1            | R[n,e,(2 x)]     = x
X[4, (_ x)]         = <F[x]>            | R[n,e,x]         = x
                                        |
L[i,n,e,(1 v b)] = I[_,e,i] <- R[n,e,v] | P[p,l,a,n,(f x)]   = (a f x)
                   L[i+1,n,e,b]         | P[p,l,a,n,<x>]     = (p x)
L[_,n,e,x]       = R[n,e,x]             | P[p,l,a,n,{n a b}] = (l n a b)
                                        | P[p,l,a,n,x:@]     = (n x)
B[a,n,e,b,(1 _ k)] = B[a,n+1,(e <>),b,k]|
B[a,n,e,b,x]       = L[a,n,e,b]         | W[n,a,b] | n>0 = {n,a,b}
```

## Law body EDSL

PLAN contains an embedded domain-specific language (EDSL) for defining law bodies. This EDSL is not a separate language per se, but rather a specific way of using PLAN constructs to represent law bodies. The B, L and R operations together define the semantics of this EDSL. Before going into these, it is informative to look at the EDSL for law bodies in the abstract. It doesn’t have a syntax, but if it did, it would look something like this:

```unset
LawBody ::= Self                   -- self-reference
          | Var Nat                -- var-reference (index into environment)
          | Literal PLAN           -- produce literal value
          | Apply LawBody LawBody  -- function application
          | let LawBody in LawBody -- let binding (append to environment)
```

The EDSL and PLAN proper are tightly intertwined:

Within a law body, you can always drop back into regular PLAN either by using the `Literal` construct (encoded using 2), or automatically when a part of the law body doesn't have meaning in the EDSL.
Conversely, PLAN switches into the EDSL whenever executing a law.

This dual-language structure allows PLAN to maintain a simple core while providing more expressive power for defining functions.


## Evaluation in detail

We'll look at each of the operations that appear the PLAN spec. But first, we have to cover "black holes":

### Black Holes (`<>`)

`<>` are temporary placeholders used during evaluation to handle recursive bindings and thunks. They serve several purposes:

- As mutable placeholders during environment building
- During evaluation of null-ary laws (E operation)
- As detection for cyclic evaluations:

**Black holes only exist during evaluation and should never appear in final normalized values.**

### `X` (function eXecution)

The relevant part of the pseudocode specification is the X operation, short for "eXecute":

```unset
X[0, (_ n a b)]     = W[N[n],N[a],F[b]]
X[1, (_ p l a n x)] = P[p,l,a,n,E[x]]
X[2, (_ z p x)]     = C[z,p,N[x]]
X[3, (_ x)]         = N[x]+1
X[4, (_ x)]         = <F[x]>
X[(f x), e]         = X[f,e]
X[<p>, e]           = X[p,e]
X[{n a b}, e]       = B[a,a,e,b,b]
```

X takes two arguments, a function and an environment that it should be executed in. Most of the time, the environment simply is simply the function itself followed by all its arguments. X is defined by eight different cases. The first five cases define the opcodes, the built in functions. Let’s look at them one by one.

- 0 creates a law. (0 n a b) casts n and a to nats, forces recursive evaluation of b, and creates a law with name n, arity a and body b. See also the [W  operation](#w-write-law).
- 1 reflects by pattern matching. (1 p l a n x) evaluates an arbitrary PLAN value x, inspects the result, and depending on its shape, chooses one of the branches p, l, a or n. See also the [P operation](#p-pattern-match).
- 2 is structural recursion on nats. (2 z p x) casts an arbitrary PLAN value x to a nat, and branches on whether the result is 0 or not. See also the [C operation](#c-recursion-on-naturals).
- 3 is increment. (3 x) casts x to a nat and increments the result by 1.
- 4 pins a value. (4 x) forces normalization of x and puts it inside a pin. All realistic interpreters will also hash x and, if a value with the same hash already exists, deduplicate it.

The three final cases define what it means to execute functions that aren’t built-in.
- If the function is an app (f x), the argument x is already included in the environment (remember that apps are closures; they define the environment), so we ignore it and simply recurse deeper to execute the actual function.
- If the function is a pin, executing it is the same as executing the function inside the pin.
- If the function is a law, we need to interpret the law body. This starts by building the environment, which is done using the B operation. Note that the law’s name isn’t used at all, it is only there to increase readability.

### `B` (Build law environment)

```unset
B[a,n,e,b,(1 _ k)] = B[a,n+1,(e <>),b,k]
B[a,n,e,b,x]       = L[a,n,e,b]
```

This operation builds the environment for executing a law body by processing any let bindings. It takes four arguments:

- a: The initial arity/position for binding values
- n: The current size of the environment
- e: The current environment
- b: The law body to evaluate
- An optional fifth argument for let binding cases

It works recursively:

- When encountering a let binding (1 _ k), it:
  - Extends the environment with a black hole placeholder (`<>`)
  - Increments the environment size
  - Recursively processes the rest of the bindings in k
- For non-let-binding values, passes control to L to evaluate the final body

The black holes serve as mutable placeholders that will be filled in during evaluation by the L operation.

### `R` (Run a law)

This EDSL for law bodies is implemented by the R operation:

```unset
R(n,e,b:@) | b≤n = I(b,e,(n-b))         # if b=0 Self, otherwise Var b
R(n,e,(0 f x))   = (R(n,e,f) R(n,e,x))  # Apply f x
R(n,e,(1 v b))   = L(n,e,v,b)           # let v in b
R(n,e,(2 x))     = x                    # Literal x
R(n,e,x)         = x                    # Literal x (fallback, incl b>n)
```

It's important to note that none of these patterns reduce under the normal PLAN evaluation rules defined by the X operation. The arities of the opcodes 0, 1 and 2 in the apps aren’t saturated, so the apps are kept as inert values. This allows any valid expression in the EDSL to be stored using this encoding.

### `E` (Evaluate)

```unset
E(o:@)     = o
E(o:<x>)   = o
E(o:(f x)) =
   E(f)
   when A(f)=1
       o <- X(o,o)
       E(o)
   o
E(o:{n a b}) =
    if a!=0 then o else
       o <- <>
       o <- R(0,o,b)
       E(o)
```

This evaluates an arbitrary PLAN expression to weak-head normal form and _replaces_ it with the result in memory. This is important! E is a stateful operation. If the evaluated PLAN expression occurs in several locations on the heap due to structural sharing, all of them will change. This is how PLAN implements thunks.

### `F` (Force normalize)

```unset
F(o) =
    E(o)
    when o:(f x)
         F(f); F(x)
    o
```

This is a helper operation that X uses to implement opcode 4. It recursively forces evaluation of an arbitrary PLAN expression, putting it into normal form.

### `P` (Pattern match)

```unset
P(p,l,a,n,(f x))   = (a f x)
P(p,l,a,n,<x>)     = (p x)
P(p,l,a,n,{n a b}) = (l n a b)
P(p,l,a,n,x:@}     = (n x)
```

This is a helper operation that X uses to implement opcode 1. It branches based on the structure of the PLAN value that is being inspected, passing the components to the appropriate continuation. This is what makes PLAN a fully reflective system.

### `N` (cast to Nat)

```unset
N(o) = E(o); if o:@ then o else 0
```

This is a helper operation that X uses to implement opcodes 0 and 3. It evaluates a PLAN expression and casts the result to a nat by simply returning 0 for non-nat values.

### `I` (index

```unset
I(f, (e x), 0) = x
I(f, e,     0) = e
I(f, (e x), n) = I(f, e, n-1)
I(f, e,     n) = f
```

This is a helper operation used by A and R. It takes a fallback case, an environment to index into – typically a nested app tree – and an index. It’s defined by recursion on the environment: if the index is 0, we’ve reached the bottom and should return either the tail or, if that’s not possible, the whole environment. If the index is greater than 0, we recur and decrement the index. If we can’t recur, we’re out of bounds and return the fallback case.

It’s worth noting that indices work from right to left instead of from left to right. This is because apps associate to the left. We could go in the other direction, but this would complicate the specification significantly without making any fundamental difference. Additionally, realistic implementations will have optimized nested app trees to arrays either way, so this change wouldn’t make implementation easier.

### `A` (Arity)

```unset
A((f x))     = A(f)-1
A(<p>)       = A(p)
A({n a b})   = a
A(n:@)       = I(1, (3 5 3), n)
```

This is a helper operation used by E. It simply returns the arity of a PLAN expression if interpreted as a function. The first three cases should be obvious. The arities of nats are hardcoded according to this table, which is implemented by the call to I, where 1 is a default value.

| nat | arity | comment               |
| --- | ----- | --------------------- |
| 0   | 3     | opcode “create a law” |
| 1   | 5     | opcode “reflect”      |
| 2   | 3     | opcode “is zero?”     |
| 3   | 1     | opcode “increment”    |
| 4   | 1     | opcode “make pin”     |
| n   | 1     | crash with message m  |

### `L` (Let-binding)

```unset
L(n,e,v,b) =
    x := <>
    f := (e x)
    x <- R(n+1,f,v)
    R(n+1,f,b)
```

This is a helper operation that is mutually recursive with R. It takes:

- i: The current binding position index (for multiple let bindings)
- n: The environment size
- e: The environment to extend with new value v
- v: The value being bound (in let cases)
- b: law body which should run against the environment after it has been extended with v

For non-let cases, it simply runs the body in the current environment using R.

### `C` (reCursion on naturals)

```unset
C[z,p,n] = if n=0 then z else p (n-1)
```

This is a helper operation that X uses to implement opcode 2. It implements primitive recursion on natural numbers, taking three arguments:

- A base case z to return when n=0
- A successor case function p to apply to (n-1) when n>0
- A natural number n to recurse on

This allows PLAN to implement structural recursion without needing explicit recursion primitives in the core language. When opcode 2 is executed via X[2, (_ z p x)], it evaluates x to a natural number and then uses C to either return the base case z (if x=0) or apply p to x-1 (if x>0).

### `S` (Saturate)

```unset
S[o:(f x y)]       = (S[(f x)] y)
S[o:(<{n a b}> y)] = o
S[o:(<f> x)]       = S[(f x)]
S[o]               = o
```

This is a helper operation that E uses during evaluation. It handles saturation of function applications - determining when a function has received enough arguments to evaluate. The rules handle several cases:

- For nested applications (f x y), recurse left-associatively by first saturating (f x) and then applying y
- For a pinned law being applied `(<{n a b}> y)`, preserve the entire expression
- For a pinned function being applied `(<f> x)`, recurse into the application
- For any other value, return it unchanged

S is called during evaluation when a function of arity 1 is encountered, ensuring proper handling of nested applications and pinned values before execution continues.

### `W` (Write law)

```unset
W[n,a,b] | n>0 = {n,a,b}
```

This is a helper operation that X uses to implement opcode 0 (law creation). It constructs a law value only if the law's name is a positive natural number, enforcing a basic validation rule on law definitions. The operation takes three arguments:

- A natural number n for the law's name, which must be >0
- A natural number a specifying the law's arity
- A value b containing the law's body

When opcode 0 is executed via X[0, (_ n a b)], it first normalizes its arguments using N[n], N[a], and F[b] respectively, then attempts to construct a law using W. If the normalized name is 0 or negative, the operation will diverge according to the "Unmatched patterns diverge" rule, preventing the creation of invalid laws.
The validation that n>0 ensures all laws in the system have proper positive natural number identifiers, which may be important for the reflection capabilities provided by opcode 1.
