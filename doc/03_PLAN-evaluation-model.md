---
sidebar_position: 3
id: plan-evaluation-model
title: PLAN; the evaluation model
---

# PLAN Evaluation model

So far we’ve only looked at PLAN as inert data. We’ve hinted at dynamics, but we haven’t given any details. Let’s go through the rest of the specification chunk by chunk.

## Opcodes

### Opcode 0 (create a law)

```unset
(0 n a b) | NAT(a)>0 = {NAT(n) NAT(a) force(b)}
```

0 creates a law. It casts n and a to nats, forces recursive evaluation of b, and creates a law with name n, arity a and body b. This allows code to construct code. force here is simply saying that the entire body should be recursively evaluated, while NAT is a helper function that returns 0 for all non-nat values:

```unset
NAT(x:@) = x  
NAT(_)   = 0
```

### Opcode 1 (reflect)

```unset
(1 p _ _ _ <x>)      = (p x)  
(1 _ l _ _ {n a b})  = (l n a b)  
(1 _ _ a _ (f x))    = (a f x)
(1 _ _ _ n x:@)      = (n x)
```

1 reflects on any PLAN value by pattern matching. Depending on whether the last argument is a pin, law, app or nat, the appropriate continuation will be chosen and evaluated. This is what makes PLAN a fully reflective system.

### Opcode 2 (nat iteration)

```unset
(2 z p x)            = n=NAT(x); if n=0 then z else p (n-1)
```

2 is nat iteration. It allows us to iterate up to x times. The last argument is cast to a nat, and depending on whether the result is 0 or not, we either default to the base case or run the successor case.

### Opcode 3 (increment)

```unset
(3 x)                = NAT(x)+1
```

3 increments. It casts x to a nat and adds 1. 

### Opcode 4 (create pin)

```unset
(4 x)                = <force(x)>
```

4 creates a pin. It forces recursive evaluation of x and wraps the result in a pin. 

## Law execution

```unset
(f:{n a b} x0..xn)   = EXEC([f x0..xn], b)  
(f:<{n a b}> x0..xn) = EXEC([f x0..xn], b)
```

Laws are executed by applying the EXEC function to an environment containing the law and its arguments, along with the law's body. This applies to both unpinned laws and pinned laws.

The EXEC helper function uses an environment to execute law bodies, which are written in an embedded domain-specific language (EDSL). This EDSL is not a separate language per se, but rather a specific way of using PLAN constructs to represent law bodies – PLAN expressions that will have access to an environment of a specific size. Before going into EXEC, it is informative to look at the EDSL for law bodies in the abstract. It doesn’t have a syntax, but if it did, it would look something like this:

```unset
LawBody ::= Self                   -- self-reference  
          | Var Nat                -- var-reference (index into environment)  
          | Literal PLAN           -- produce literal value  
          | Apply LawBody LawBody  -- function application  
          | let LawBody in LawBody -- let binding (append to environment)
```

These constructs are implemented by EXEC in the following way:

#### *References*

```unset
EXEC(e, n:@) | n<len(e) = e[n]
```

In law bodies, variables are referenced using indices instead of by name. 0 references the law itself, while all other valid indices reference the law’s arguments. (Remember that before calling EXEC, we constructed the environment with the law at the head.) This corresponds to the Self and Var n constructs in the fake EDSL syntax.

#### *Function application*

```unset
EXEC(e, (0 x y))        = (EXEC(x) EXEC(y))
```

The Apply construct is represented by 0. This continues to execute two law bodies, and applies the first to the second.

#### *Let-binding*

```unset
EXEC(e, (1 v b))        = EXEC(f,b) where f = e ++ [EXEC(f,v)]
```

Let-bindings are represented by 1. This creates a lazy binding – the value v is evaluated in an environment that includes the binding itself, allowing for cyclic data structures. The environment is extended with this new binding before evaluating the body b.

#### *Literals*

```unset
EXEC(e, (2 x))          = x  
EXEC(e, x)              = x
```

A law body can produce a literal PLAN value, either deliberately using 2 or by default. These will always have been normalized during the creation of the law using opcode 0, and so will only reduce if they’re further applied to something within the law body.

#### Pin application

```unset
(<i> ..)             = (i ..)
```

If a pin is applied to an argument, that’s the same thing as just applying the PLAN value inside the pin directly. This reflects the fact that formally, pins are simply magic boxes that contain normalized values.

## Evaluation order

The informal PLAN specification contains these instructions:

```unset
Treat this as a combinator system, and use normal-order evaluation to normalize.
Unmatched patterns diverge.
```

These are simply saying that PLAN evaluation is lazy, and that if we encounter a pattern that doesn’t match any rules, then we should crash.

## Seed (de/serialization)

Since we imagine Pallas as a natively networked database VM, we want to be able to write arbitrary PLAN values – including closures and suspended thunks – to disk, or send them over the network. Any PLAN value can be serialized to a nat using a format called **seed**, which is a simple format for encoding binary trees of natural numbers.

Seed has a number of desirable properties:

- It's very simple to write an efficient SEED loader. (See [this demo](TK TODO link) for an example of this).  
- Naturals are stored word-aligned and word-padded. Multi-word nats can be loaded with zero copies by just storing big numbers as pointers into the input buffer. This is especially useful when working with files that are loaded with `mmap()` and contain large binary blobs. In this situation, the host OS will transparently page this data in-and-out as necessary.  
- All repeated subtrees are written only once.

Note again that PLAN trees encoded with seed can contain *computations*, not just values. If this is undesirable, the decoder can just verify that everything it reads is in normal form.

## Loot (disassembly)

PLAN values can be disassembled and displayed in a human-readable form using the **loot** [algorithm](TK link to loot). For example, below is the disassembled PLAN of the foldr function, including all functions it has inlined into its body:

```unset
* (_If a b c)=(2 c ((d e & d) b) a)  
* (_Not a)=(_If a 0 1)  
* (_IsApp a)=(1:(b&0):(b c d & 0):(b c & 1):(b&0) a)  
* (_Null a)=(_Not _IsApp-a)  
* (_Cdr a)=(1:(b&b):(b c d & d):(b c & c):(b&0) a)  
* (_Car a)=(1:(b&4):(b c d & 0-b-c):(b c & b):(b&0) a)  
* (_Get a b)=(_If _Null-a 0 (2 _Cdr-a (_Get _Car-a) b))  
* (id a)=a  
* (_Dec a)=(2:0:id a)  
* (_Seq a b)=(2 b ((c d & c) b) a)  
*   = (_Len a)  
    ^ a  
    : (go b c ? _Seq b (_If _Null-c b (go 3-b _Car-c)))  
    : 0  
*   = (foldr a b c)  
    ^ a c b 0 _Len-c  
    ? (go d e f g h)  
    (_If h (d _Get-e-g (go d e f 3-g _Dec-h)) f)
```
