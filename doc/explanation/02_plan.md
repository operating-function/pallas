---
title: PLAN
---

The previous section provided an overview of the system and how the different components contribute to its overall functioning. Let’s now take a closer look at each component, starting with PLAN.

# PLAN

Nearly every core innovation of Pallas emerges from the design of PLAN. PLAN is the “bytecode” of Pallas, but in contrast to bytecode, it is human readable. It is a Turing-complete evaluation model which is purely functional, lazy and reflective. It is designed to strike a reasonable balance between several different design requirements:

1. **Human Readability**: Unlike typical low-level formats, PLAN maintains a degree of readability that allows developers to inspect and understand "binaries" directly.  
2. **Functional Compile Target**: PLAN serves as an excellent target for functional language compilers.  
3. **Efficient Memory Representation**: The structure of PLAN is designed to map well to in-memory data structures, allowing for efficient execution.  
4. **Database Oriented**: PLAN is purpose-built for personal databases, offering a “single-level store”.  
5. **Ease of Implementation**: PLAN is possible to implement efficiently in a few thousand lines of code.  
6. **Extensibility and Modifiability**: PLAN allows programs to both construct new programs and inspect existing ones – including themselves. This allows them to upgrade and extend themselves, effectively running forever.

It's not necessary to understand PLAN to program Pallas, but if you understand PLAN, you'll understand at least half of the system. Here is an informal pseudocode specification of PLAN, followed by an explanation:

```unset
Each value is a pin x:<i>, a law x:{n a b}, an app x:(f g), or a nat x:@.

Treat this as a combinator system, and use normal-order evaluation to normalize.
Unmatched patterns diverge.

    (0 n a b) | NAT(a)>0 = {NAT(n) NAT(a) force(b)}  
    (1 p _ _ _ <x>)      = (p x)  
    (1 _ l _ _ {n a b})  = (l n a b)  
    (1 _ _ a _ (f x))    = (a f x)  
    (1 _ _ _ n x:@)      = (n x)  
    (2 z p x)            = n=NAT(x); if n=0 then z else p (n-1)  
    (3 x)                = NAT(x)+1  
    (4 x)                = <force(x)>  
    (f:{n a b} x0..xn)   = EXEC([f x0..xn], b)  
    (f:<{n a b}> x0..xn) = EXEC([f x0..xn], b)  
    (<i> ..)             = (i ..)

    NAT(x:@) = x  
    NAT(_)   = 0

    EXEC(e, n:@) | n<len(e) = e[n]  
    EXEC(e, (0 x y))        = (EXEC(x) EXEC(y))  
    EXEC(e, (1 v b))        = EXEC(f,b) where f = e ++ [EXEC(f,v)]  
    EXEC(e, (2 x))          = x  
    EXEC(e, x)              = x
```

This informal specification glosses over some details regarding evaluation order and normalization, but is otherwise correct. For a more detailed and implementation-oriented specification, see the [reference](reference/plan/plan).

To start, we can note that the specification contains three things: at the top there’s a data model, which is then followed by a few informal instructions and finally a number of rewrites over the data model. We’ll go through the data model, then the rewrite rules, and finally the informal instructions.

