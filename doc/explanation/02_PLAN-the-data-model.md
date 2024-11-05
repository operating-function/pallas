---
sidebar_position: 2
id: plan-data-model
title: Data Model
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

## Data model

PLAN’s data model is specified abstractly as:

`Each value is a pin x:<i>, a law x:{n a b}, an app x:(f g), or a nat x:@.`

These four constructs are what give PLAN its name, and they abstractly represent the following data structure:

Every PLAN expression is a heap that consists of a merkle-DAG of subheaps. Every subheap is a tree structure containing absolutely pure n-ary functions, closures, natural numbers, and pointers to other subheaps. We represent our heap as a merkle-DAG because its hash-based nature gives us pointers that remain stable between VM runs, allowing us to support huge heaps by transparently paging subheaps in and out from disk. This allows users to e.g. keep their whole media library “in memory”.

::::warning[TODO]
Diagram forthcoming
::::

There’s a lot to unpack in that description, so let’s go through the four constructs one by one.

## Pins

**Subheaps: content addressed DAG nodes and contiguous memory regions of normalized values**

Pins are heaps, and nodes in the merkle-DAG of heaps. Formally, a pin `<i>` is just a magic box that contains another PLAN value `i` that is guaranteed to have been normalized. In practice, a pin is a hint to the virtual machine that it should hash and deduplicate i, and store it in a contiguous region in memory and on disk, similar to [GHC’s Compact regions](https://harpocrates.github.io/ghc-head-libraries/ghc-compact/GHC-Compact.html). This gives us several nice properties:

- Comparing two pins for equality is a constant-time operation, similar to when e.g. a string has been interned.  
- Data access inside a pin is fast, since contiguous memory regions are cache-friendly.  
- On disk and over the network, pins can be addressed by their hashes, which allows them to be loaded on demand, making the heap stable between runs. In this way they are similar to memory pages that might reference each other, but since pins are always normalized, these references are acyclic.  
- PLAN values are immutable, so we need to reclaim memory somehow. Garbage collection requires a full heap-traversal, which is a problem with the huge heaps that we want to support. Pins mitigate this problem because each one is stored in a contiguous region on disk, which means that pins on disk only ever reference other pins, not values *inside* other pins. Because of this, a full garbage collection pass only needs to traverse the pin DAG, without looking at the data inside the pins. PLAN evaluation is lazy, but each state-machine transition does a full normalization, so long-lived space leaks are impossible, and the on-disk representation will never contain unevaluated thunks.

   

## Laws 

**Supercombinators: pure n-ary functions**

The law `{n a b}` is a supercombinator of arbitrary but fixed arity a, with the body b and the name n. “[Supercombinator](https://wiki.haskell.org/Super\_combinator)” really just means “an *actually* pure function (or a constant)”. Even in supposedly pure programming languages, functions can typically call other functions that are “in scope”, which means that they can access implicit state – a very reasonable UX affordance. But PLAN is not a UI, it is a specification, so we can’t tolerate such ambiguity. To make a law accessible to another law, it must either be inlined or passed as an argument. This is what we mean when we say that laws are *actually* pure functions. The only environment a law has access to is itself and its arguments.

## Apps

**Applications: closures or thunks**

Apps are applications of functions to arguments. They are binary trees or cons cells that are often left-heavy or left-skewed, such as (((f a) b) c). Syntactically they associate to the left, so we can also write (f a b c). As the variable names suggest, the leftmost node in an app is typically a function, while the rest are arguments to that function, some of which might be other apps.

A partially applied function, such as `({"fun" 2 body} arg1)` is a closure (notice the arity is 2), while a fully applied function such as `({"fun" 2 body} arg1 arg2)` is a thunk. Apps that are closures can be deconstructed and inspected, but once an app has graduated to a thunk, it will be reduced before it can be further manipulated.

Since all functions are *actually* pure, a thunk-app contains the entire environment needed to execute a function. Because apps are used as environments in this way, the interpreter typically improves memory locality by recognizing their left-associativity and optimizing them to arrays, instead of storing the formal tree structure.

## Nats

**Natural numbers: opaque data or opcodes**

Nats are natural numbers. Most of the time this is just data. We encode integers, strings and MP3 files using natural numbers. Nats that are smaller than the machine’s word size get stored directly. Bigger nats are stored as pointers and benefit from structural sharing. The nats 0-4 can also code for opcodes: instructions to the virtual machine. Like laws, these operations are pure functions, but they are built in.
