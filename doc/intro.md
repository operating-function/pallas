---
sidebar_position: 1
---

# System Overview

Pallas is a stack of technologies. At the bottom lies **PLAN**, an evaluation model. On top of PLAN we have two separate stacks: on one hand **Rex** and **Sire** allow programmers to implement programming languages which compile to PLAN, while a **manifest** format consisting of **cogs** and **drivers** is used to completely specify the behavior of a virtual machine that runs PLAN. We’ll start by explaining how these fit together on a high level, and then go into each one in detail.

At the top, the programmer is offered a programming environment that’s similar to Erlang/OTP, the EVM, and Lisp. Like Erlang/OTP, the programming paradigm is functional, there’s native support for concurrency based on message passing, and programs can be hot reloaded without downtime. Like the EVM, program state is automatically persisted – programs can just keep everything in memory and will run forever (but there is no global consensus). Like Lisp, programs can universally introspect themselves and their environment, and dynamically introduce new code.

At the bottom lies a concurrency-oriented virtual machine whose behavior is completely specified by a user-supplied manifest. The manifest is specified using a standardized format which the virtual machine knows how to interpret and update during operation. This standardized format allows you to move your manifest to a different virtual machine on a different device, and pick up where you left off. The format is guaranteed to be backward compatible, so that programs continue to work even in the future. We aim to also maximize forward compatibility, so that an old machine can come online, receive code updates over the network, and continue to operate with as little degradation of service as possible.

The manifest is encoded in a format called PLAN. PLAN is a lambda-inspired combinator system which is purely functional, lazy, reflective, and has been optimized for data storage. It is very minimal and easy to implement efficiently. PLAN is used as a standard format for arbitrary compute and data, and manifests code for PLAN values of a particular shape, which codes for both compute and managed side effects.

PLAN is human-readable but not human-writeable in practice. To alleviate this, a minimal bootstrapping compiler of a more usable language called Sire is made available, both as Sire source code and as compiled but human-auditable PLAN. Sire is very bare-bones, and is simply intended to be used to implement more capable languages.

Sire uses a syntax called Rex, for R-expressions. Rex is to Pallas as S-expressions are to Lisp: a single highly regular syntax that can be used by many different languages. Rex is almost as simple as pure S-expressions, and simpler than the actual syntaxes of all major Lisps, while being significantly more flexible: Rex can look very much like Python, Haskell or YAML. Using Rex, new languages can be defined as macros on top of Sire, simply rebinding its syntax to new semantics. This allows the system to be flexibly extended while reusing the lexer, parser and pretty-printer.

Taken together, these components give us a resilient, extensible and trust-minimized system that manages persistence, concurrency, portability and real-world interaction in a single unified model. We believe that this will be useful for use cases as diverse as home media servers, AI agent orchestrators, and as an implementation substrate and data syncing layer for p2p protocols.

# Technical stack overview

The previous section provided an overview of the system and how the different components contribute to its overall functioning. Let’s now take a closer look at each component.

## PLAN

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

This informal specification glosses over some details regarding evaluation order and normalization, but is otherwise correct. For a more detailed and implementation-oriented specification, see the [reference](TK TODO link).

To start, we can note that the specification contains three things: at the top there’s a data model, which is then followed by a few informal instructions and finally a number of rewrites over the data model. We’ll go through the data model, then the rewrite rules, and finally the informal instructions.

### Data model

PLAN’s data model is specified abstractly as:

`Each value is a pin x:<i>, a law x:{n a b}, an app x:(f g), or a nat x:@.`

These four constructs are what give PLAN its name, and they abstractly represent the following data structure:

Every PLAN expression is a heap that consists of a merkle-DAG of subheaps. Every subheap is a tree structure containing absolutely pure n-ary functions, closures, natural numbers, and pointers to other subheaps. We represent our heap as a merkle-DAG because its hash-based nature gives us pointers that remain stable between VM runs, allowing us to support huge heaps by transparently paging subheaps in and out from disk. This allows users to e.g. keep their whole media library “in memory”.

[TK diagram]

There’s a lot to unpack in that description, so let’s go through the four constructs one by one.

#### Pins (subheaps: content addressed DAG nodes and contiguous memory regions of normalized values)

Pins are heaps, and nodes in the merkle-DAG of heaps. Formally, a pin `<i>` is just a magic box that contains another PLAN value `i` that is guaranteed to have been normalized. In practice, a pin is a hint to the virtual machine that it should hash and deduplicate i, and store it in a contiguous region in memory and on disk, similar to [GHC’s Compact regions](https://harpocrates.github.io/ghc-head-libraries/ghc-compact/GHC-Compact.html). This gives us several nice properties:

- Comparing two pins for equality is a constant-time operation, similar to when e.g. a string has been interned.  
- Data access inside a pin is fast, since contiguous memory regions are cache-friendly.  
- On disk and over the network, pins can be addressed by their hashes, which allows them to be loaded on demand, making the heap stable between runs. In this way they are similar to memory pages that might reference each other, but since pins are always normalized, these references are acyclic.  
- PLAN values are immutable, so we need to reclaim memory somehow. Garbage collection requires a full heap-traversal, which is a problem with the huge heaps that we want to support. Pins mitigate this problem because each one is stored in a contiguous region on disk, which means that pins on disk only ever reference other pins, not values *inside* other pins. Because of this, a full garbage collection pass only needs to traverse the pin DAG, without looking at the data inside the pins. PLAN evaluation is lazy, but each state-machine transition does a full normalization, so long-lived space leaks are impossible, and the on-disk representation will never contain unevaluated thunks.

   

#### Laws (supercombinators: pure n-ary functions)

The law `{n a b}` is a supercombinator of arbitrary but fixed arity a, with the body b and the name n. “[Supercombinator](https://wiki.haskell.org/Super\_combinator)” really just means “an *actually* pure function (or a constant)”. Even in supposedly pure programming languages, functions can typically call other functions that are “in scope”, which means that they can access implicit state – a very reasonable UX affordance. But PLAN is not a UI, it is a specification, so we can’t tolerate such ambiguity. To make a law accessible to another law, it must either be inlined or passed as an argument. This is what we mean when we say that laws are *actually* pure functions. The only environment a law has access to is itself and its arguments.

#### Apps (applications: closures or thunks)

Apps are applications of functions to arguments. They are binary trees or cons cells that are often left-heavy or left-skewed, such as (((f a) b) c). Syntactically they associate to the left, so we can also write (f a b c). As the variable names suggest, the leftmost node in an app is typically a function, while the rest are arguments to that function, some of which might be other apps.

A partially applied function, such as `({"fun" 2 body} arg1)` is a closure (notice the arity is 2), while a fully applied function such as `({"fun" 2 body} arg1 arg2)` is a thunk. Apps that are closures can be deconstructed and inspected, but once an app has graduated to a thunk, it will be reduced before it can be further manipulated.

Since all functions are *actually* pure, a thunk-app contains the entire environment needed to execute a function. Because apps are used as environments in this way, the interpreter typically improves memory locality by recognizing their left-associativity and optimizing them to arrays, instead of storing the formal tree structure.

#### Nats (natural numbers: opaque data or opcodes)

Nats are natural numbers. Most of the time this is just data. We encode integers, strings and MP3 files using natural numbers. Nats that are smaller than the machine’s word size get stored directly. Bigger nats are stored as pointers and benefit from structural sharing. The nats 0-4 can also code for opcodes: instructions to the virtual machine. Like laws, these operations are pure functions, but they are built in.

### Evaluation model

So far we’ve only looked at PLAN as inert data. We’ve hinted at dynamics, but we haven’t given any details. Let’s go through the rest of the specification chunk by chunk.

#### Opcode 0 (create a law)

```unset
(0 n a b) | NAT(a)>0 = {NAT(n) NAT(a) force(b)}
```

0 creates a law. It casts n and a to nats, forces recursive evaluation of b, and creates a law with name n, arity a and body b. This allows code to construct code. force here is simply saying that the entire body should be recursively evaluated, while NAT is a helper function that returns 0 for all non-nat values:

```unset
NAT(x:@) = x  
NAT(_)   = 0
```

#### Opcode 1 (reflect)

```unset
(1 p _ _ _ <x>)      = (p x)  
(1 _ l _ _ {n a b})  = (l n a b)  
(1 _ _ a _ (f x))    = (a f x)
(1 _ _ _ n x:@)      = (n x)
```

1 reflects on any PLAN value by pattern matching. Depending on whether the last argument is a pin, law, app or nat, the appropriate continuation will be chosen and evaluated. This is what makes PLAN a fully reflective system.

#### Opcode 2 (nat iteration)

```unset
(2 z p x)            = n=NAT(x); if n=0 then z else p (n-1)
```

2 is nat iteration. It allows us to iterate up to x times. The last argument is cast to a nat, and depending on whether the result is 0 or not, we either default to the base case or run the successor case.

#### Opcode 3 (increment)

```unset
(3 x)                = NAT(x)+1
```

3 increments. It casts x to a nat and adds 1. 

#### Opcode 4 (create pin)

```unset
(4 x)                = <force(x)>
```

4 creates a pin. It forces recursive evaluation of x and wraps the result in a pin. 

#### Law execution

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

##### *References*

```unset
EXEC(e, n:@) | n<len(e) = e[n]
```

In law bodies, variables are referenced using indices instead of by name. 0 references the law itself, while all other valid indices reference the law’s arguments. (Remember that before calling EXEC, we constructed the environment with the law at the head.) This corresponds to the Self and Var n constructs in the fake EDSL syntax.

##### *Function application*

```unset
EXEC(e, (0 x y))        = (EXEC(x) EXEC(y))
```

The Apply construct is represented by 0. This continues to execute two law bodies, and applies the first to the second.

##### *Let-binding*

```unset
EXEC(e, (1 v b))        = EXEC(f,b) where f = e ++ [EXEC(f,v)]
```

Let-bindings are represented by 1. This creates a lazy binding – the value v is evaluated in an environment that includes the binding itself, allowing for cyclic data structures. The environment is extended with this new binding before evaluating the body b.

##### *Literals*

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

#### Evaluation order

The informal PLAN specification contains these instructions:

```unset
Treat this as a combinator system, and use normal-order evaluation to normalize.
Unmatched patterns diverge.
```

These are simply saying that PLAN evaluation is lazy, and that if we encounter a pattern that doesn’t match any rules, then we should crash.

### Seed (de/serialization)

Since we imagine Pallas as a natively networked database VM, we want to be able to write arbitrary PLAN values – including closures and suspended thunks – to disk, or send them over the network. Any PLAN value can be serialized to a nat using a format called **seed**, which is a simple format for encoding binary trees of natural numbers.

Seed has a number of desirable properties:

- It's very simple to write an efficient SEED loader. (See [this demo](TK TODO link) for an example of this).  
- Naturals are stored word-aligned and word-padded. Multi-word nats can be loaded with zero copies by just storing big numbers as pointers into the input buffer. This is especially useful when working with files that are loaded with `mmap()` and contain large binary blobs. In this situation, the host OS will transparently page this data in-and-out as necessary.  
- All repeated subtrees are written only once.

Note again that PLAN trees encoded with seed can contain *computations*, not just values. If this is undesirable, the decoder can just verify that everything it reads is in normal form.

### Loot (disassembly)

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

## VM and Interpreter

PLAN is a standard for representing compute and data in the abstract; a PLAN expression is a specification of a program. We need to be able to turn such specifications into actual hardware operations. This is the job of a PLAN interpreter. A PLAN interpreter implements PLAN.

Additionally, since we’re promising a concurrent and persisted virtual machine that should be able to affect the real world, we need a standard that tells us how to instantiate such a VM based on a PLAN manifest.

Let’s talk about interpreting PLAN first, then we’ll move on to how manifests specify the operation of the VM.

### Interpreting PLAN

While the [informal PLAN specification](TK TODO link) is shown in this explanation, the [formal specification](TK TODO link) is purposefully written in a very implementation-oriented form, to emphasize that this is a wholly practical system despite being so minimal. Most of the implementation falls out directly from that specification. However, some optimizations are necessary to make a PLAN interpreter practically useful. In the [explanation of PLAN’s data model](TK TODO link), we’ve explained how the memory layout of pins and apps can be optimized and how the former helps with garbage collection, but we also need to consider jets and data jets.

#### Jets

PLAN only has a single arithmetic operation: increment by one, also known as the opcode 3. This allows us to do anything we want in theory: we can implement addition as repeated increment, and multiplication as repeated addition, and so on. This works in principle, but not in practice.

The reason why PLAN doesn’t have more than 5 opcodes is because it needs to be forward and backward compatible. Old code should work on new implementations, and new code should work on old implementations, so we can’t add new opcodes to the standard. Because of this, we opt to express all operations using *algorithms* instead of extending the evaluation model. If an operation turns out to be beneficial to optimize, the PLAN interpreter is free to do so by short-circuiting PLAN evaluation and instead performing the same operation some other way. We call such optimized functions **jets**, and they’re similar to how a CPU might dispatch certain operations to a GPU – indeed, a jet could be used to replace a matrix multiplication algorithm with a direct hardware instruction.

A jet is always a pinned law, `<{n a b}>`. Since laws are how we store code, they are the only things that make sense to jet, but checking every law that we encounter against the interpreter’s jet table would be expensive. Pins give us constant-time equality checks, so we immediately know whether a pinned law should be jetted or not.

[Jets reference and examples](TK link to new stub page, and add stubs in general to contribution suggestions)

#### Data jets

Certain data structures are crucial for performant computation. In particular, computers love linear memory, so we need arrays. We solve this using something called **data jets**, which are simply irreducible PLAN expressions that the interpreter knows how to optimize in memory. For example, an array of length n is represented in PLAN using the following structure:

```unset
({0 n+1 0} x0 x1 … xn)
```

There are a few things to note here:

- Since the law’s arity is n+1, this app doesn’t reduce. Formally, it’s simply a closure that contains all the array elements.  
- The law has the name 0, which is null in both ASCII and UTF-8, and so in most languages it will be an illegal name for everything except perhaps anonymous functions.  
- The body is `0`, which means that the function immediately returns itself.

An anonymous function that *only* returns itself is objectively useless, so we know that we'll never encounter this law in any other context. But we do need to check whether every new law we encounter matches this pattern. Fortunately, this only requires exactly three operations: `(name==0 && body==0)`.

### Manifests

While PLAN is a Turing-complete evaluation model, we don’t just want to compute. We want to store data and perform actions in the real world. For this reason, we use PLAN to write a manifest that instructs a virtual machine how to do this. A manifest can be stored in many different but equivalent ways on disk, but in the end they all code for a PLAN expression that specifies something called a **cog**, which is responsible for keeping data and business logic intact and in sync. Among other things, a cog includes a set of **drivers**, which instruct the VM on how the cog needs to connect to the outside world. A driver could for example be a stateless HTTP server or an RTP video streamer.

Both cogs and drivers are persisted and evolving pieces of code that will resume their operation if the VM is ever rebooted, moved, or upgraded. The difference is that after a VM reboot, a cog will resume from its *latest* state, while a driver will resume from its *initial* state. The initial states of all drivers are included in the cog’s state; the cog tells the VM which drivers it requires in order to interact with the outside.

Both cogs and drivers are transition functions expressed in PLAN – closures that are ready to receive an input and will produce a new transition function as a result. Owing to their differences, cogs and drivers look slightly different from each other. We’ll start by looking at cogs.

#### Cogs

[TK diagram]

Cogs are programs that run forever. Even if you reboot your machine, a cog will continue exactly where it left off. Cogs are important because both code and data are important. The traditional persistence solution is to do a complex orchestration involving some combination of a database and source code, configuration files and binaries on a filesystem, and hope that these will allow the machine to get back to the same state at a later time. Or not – “have you tried turning it off and on again?” implies that we often aren’t able to achieve this. In contrast, cogs simply keep their state “in memory” and advance the entire system state transactionally.

PLAN doesn’t have types, and no mainstream languages have good notations for talking about closures that need to capture certain values. Conceptually though, the following pseudo-Haskell paints a reasonably clear picture of what the VM expects from a cog:

```haskell
type Worker   = Driver | Job -- Jobs are explained below
type WorkerId = Nat  
type MsgId    = Nat  
type Input    = (WorkerId, PLAN)  
type Output   = (WorkerId, MsgId, PLAN)

type CogClosure =  
    { run     :: CogClosure -> Array Input -> (CogClosure, Array Output)  
    , state   :: state  
    , query   :: WorkerID -> state -> PLAN -> PLAN  
    , workers :: Array Worker  
    }
```

A cog is a closure that captures four values:

- `run` is a function that will be called by the VM as soon as it has any inputs to give to the cog. The first argument will always be the cog closure itself, and the second argument will be a batch of inputs. run will then return a tuple with a new cog closure and an array of outputs that the VM should give to drivers.  
- `state` is a state value that the cog makes available to its drivers for concurrent read-only queries.  
- `query` is a function that drivers are forced to use when querying the cog. It accepts the driver’s worker ID, the cog’s current state and a PLAN value that codes for a query. The `query` function is allowed to interpret the PLAN value however it wants. It could even be code!  
- `workers` is an array of drivers and parallel evaluation jobs that the VM will run on behalf of the cog. We haven’t mentioned parallel evaluation jobs yet, we’ll return to them after we’ve explained drivers in more detail.

The most important takeaway here is that the cog is free to change itself. It can even replace itself with a completely new value. This means that programs can upgrade themselves and effectively run forever.

We’ve said that the VM will persist the most recent state of the cog. If a cog grows large, writing its entire PLAN value to disk (or to a cloud server!) could be quite expensive. But because cogs are transition functions, we don’t need to do that. We can simply write each batch of inputs to a log. If we need to get back to our latest state after a reboot, we can replay the log. Whenever the log grows large, we can write a snapshot of the current cog state, prune the log up until that point, and start writing a new log.

See the [cog reference](TK TODO link) for a more detailed description of this process.

#### Drivers

[TK diagram]

Drivers are programs that mediate the cog’s interactions with the outside world. They allow the cog to tell the VM how and when it should perform actions beyond pure computations. Drivers are important to minimize disk writes in a predictable and portable way. A naive architecture might feed all incoming data to the cog, but since this would incur a high number of disk writes, in practice this architecture would force the VM to implement a poorly specified exoskeleton that sifts through incoming data and decides when something should get persisted. This would almost certainly vary between runtimes, and so those performance optimizations wouldn’t be portable. In the extreme, certain network protocols that use many small packets to build up huge logical messages might only work on runtimes that are aware of these optimizations. We use drivers to ensure that we can optimize disk writes in a way that’s actually portable.

The cog puts drivers into its array of workers in order to instruct the VM how it should interact with the outside world. Drivers do not have automatically persisted states, but they do have states. They need this to be able to engage in stateful protocols such as sessions or handshakes, so that they can accumulate and assemble incoming data before submitting semantically meaningful units as inputs to the cog. 

The format for drivers closely resembles that of cogs, but is simpler. Again in Haskell:

```haskell
type MsgId  = Nat  
type Effect = (MsgId, Eff a)

type Driver = Driver -> MsgId -> PLAN -> (Driver, Array Effect)
```

Just as a cog, a driver is expected to produce a new driver and an array of outputs (here called effects, more on that in a bit). Just as a cog, the driver has access to the current version of itself when doing so, but unlike a cog, that doesn’t have to be a closure that captures any additional values – hence the simpler structure. The driver is also only passed one input (accompanied by a message ID) at a time instead of a batch, which is cheap because driver inputs are never written to disk.

The simplest version of a driver is one that simply opens a connection to the outside world and always just forwards any incoming data to the cog as an input. No state is necessary for this, so drivers aren’t required to capture any additional values in a closure the way that a cog does. But in practice, most drivers will close over some custom state, so that they can respond intelligently to incoming data. Most of the time, it’s neither efficient nor useful to indiscriminately persist all incoming data, so most realistic drivers should implement *protocols* such as HTTP, Telnet or RTP.

See the [driver reference](TK TODO link) for more information on the driver format.

##### *Effects*

[TK diagram: cog<->driver->effects<->runtime<->driver]

Drivers can ask the VM to do three things: query the cog’s current state, submit an input to the cog, and handle outside connections. They do this by emitting effects. Effects are simply arrays which contain the name of an effect together with some data. The runtime, or an in-system virtualization environment, will read this, perform some operation, and might inject an input back into the driver at a later time. 

For now, the only interface that workers have to the outside world is TCP. UDP will be added in the future. You can do basically everything over these – for example the host machine’s file system can be read and manipulated over TCP using an FTP driver. By limiting ourselves to proven network transports such as TCP and UDP, we can do everything within the system instead of having to continuously extend the runtime, thus maximizing forward compatibility and [making it easy to support new architectures](TK: link-to-philosophy).

Of course, some things require lower latency than either TCP or UDP can offer, for example real-time audio or graphics. Eventually we’ll have to add effects for such interfaces, but we expect these to be few and new ones to be very infrequent. We can’t guarantee perfect forward compatibility, but we expect it to be very good in practice.

See the [effect reference](TK TODO link) for a list of all available effects.

#### Jobs

When discussing the cog’s array of workers, we mentioned that a worker can be either a driver or a parallel evaluation **job**. While drivers handle concurrency, jobs handle parallelism. A job is simply a PLAN expression that the cog asks the VM to evaluate. Once evaluation has crashed or completed, the result will be logged and injected as an input to the cog.

See the [job reference](TK TODO link) for more information.

## Sire

Sire is a minimalist functional language that bootstraps itself from PLAN. Unlike PLAN, Sire aims to be realistic for humans to write. In service of this goal, it allows programmers to reference previously defined names in an environment, which it uses [lambda lifting](TK TODO link) to eliminate when compiling itself to PLAN.

For more information on using Sire, see the [tutorial](TK TODO link) and the [language reference](TK TODO link).

### Rationale

The primary purpose of Sire is to implement more sophisticated languages, which is important because it allows us to achieve a [trustless system](TK link to philosophy): Sire’s compiler is written in just a few thousand lines of Sire, whose syntax is similar enough to [disassembled PLAN](TK TODO link to loot) that the compiler “binary” can be audited by visual comparison to the source code. A more sophisticated language can then simply be implemented directly using Sire source. Nowhere in this chain does the user have to trust an opaque binary. This is in stark contrast to most high level languages which can only be compiled with trusted binaries which are often practically impossible to bootstrap from a high-trust foundation.

While Sire is untyped, it has built-in support for unit tests as a basic code verification measure. The language supports both reflection and reification – it can examine its environment, syntax and terms, and can dynamically construct new code, as well as extend and replace its own syntax through macros. Changing the language in this way gives us a concrete trustless path to an arbitrary number of more advanced languages with better affordances for enforcing correctness and compositionality, using types and other static analysis techniques.

### Rex

Sire uses an unusual syntax called Rex. It’s not necessary to understand Rex to use Sire, except when using macros to change or extend the syntax. Rex is a completely regular and data-oriented abstract syntax that still has a very flexible and expressive concrete syntax thanks to several different input modes. A language implementation that uses Rex would typically transform abstract Rex trees into its own AST, and then compile that to PLAN or interpret it directly, but a Rex tree also contains information on the concrete input mode used in source code.

The point of Rex is to decouple the syntax from the semantics in a way similar to how Lisps tend to use S-expressions, but to also give language developers the ability to offer programmers cleaner and more readable syntaxes than what S-expressions can support. This supports rapid iteration of language designs by removing the need to write new lexers, parsers, pretty printers, and allows optimization code to be shared.

To understand Rex, see the [reference](TK TODO link). To use it, see the [tutorial](TK TODO link) and [reference](TK TODO link) on Sire’s macro system.
