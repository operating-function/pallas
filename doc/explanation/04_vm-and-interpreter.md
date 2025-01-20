---
sidebar_position: 4
id: vm-and-interpreter
title: VM and Interpreter
---

# VM and Interpreter

PLAN is a standard for representing compute and data in the abstract; a PLAN expression is a specification of a program. We need to be able to turn such specifications into actual hardware operations. This is the job of a PLAN interpreter. A PLAN interpreter implements PLAN.

Additionally, since we’re promising a concurrent and persisted virtual machine that should be able to affect the real world, we need a standard that tells us how to instantiate such a VM based on a PLAN manifest.

Let’s talk about interpreting PLAN first, then we’ll move on to how manifests specify the operation of the VM.

## Interpreting PLAN

While the [informal PLAN specification](TK TODO link) is shown in this explanation, the [formal specification](TK TODO link) is purposefully written in a very implementation-oriented form, to emphasize that this is a wholly practical system despite being so minimal. Most of the implementation falls out directly from that specification. However, some optimizations are necessary to make a PLAN interpreter practically useful. In the [explanation of PLAN’s data model](explanation/02.5_PLAN-the-data-model.md), we’ve explained how the memory layout of pins and apps can be optimized and how the former helps with garbage collection, but we also need to consider jets and data jets.

### Jets

PLAN only has a single arithmetic operation: increment by one, also known as the opcode 3. This allows us to do anything we want in theory: we can implement addition as repeated increment, and multiplication as repeated addition, and so on. This works in principle, but not in practice.

The reason why PLAN doesn’t have more than 5 opcodes is because it needs to be forward and backward compatible. Old code should work on new implementations, and new code should work on old implementations, so we can’t add new opcodes to the standard. Because of this, we opt to express all operations using *algorithms* instead of extending the evaluation model. If an operation turns out to be beneficial to optimize, the PLAN interpreter is free to do so by short-circuiting PLAN evaluation and instead performing the same operation some other way. We call such optimized functions **jets**, and they’re similar to how a CPU might dispatch certain operations to a GPU – indeed, a jet could be used to replace a matrix multiplication algorithm with a direct hardware instruction.

A jet is always a pinned law, `<{n a b}>`. Since laws are how we store code, they are the only things that make sense to jet, but checking every law that we encounter against the interpreter’s jet table would be expensive. Pins give us constant-time equality checks, so we immediately know whether a pinned law should be jetted or not.

[Jets reference and examples](../reference/vm-and-interpreter/jets.md)

### Data jets

Certain data structures are crucial for performant computation. In particular, computers love linear memory, so we need arrays. We solve this using something called **data jets**, which are simply irreducible PLAN expressions that the interpreter knows how to optimize in memory. For example, an array of length n is represented in PLAN using the following structure:

```unset
({0 n+1 0} x0 x1 … xn)
```

There are a few things to note here:

- Since the law’s arity is n+1, this app doesn’t reduce. Formally, it’s simply a closure that contains all the array elements.  
- The law has the name 0, which is null in both ASCII and UTF-8, and so in most languages it will be an illegal name for everything except perhaps anonymous functions.  
- The body is `0`, which means that the function immediately returns itself.

An anonymous function that *only* returns itself is objectively useless, so we know that we'll never encounter this law in any other context. But we do need to check whether every new law we encounter matches this pattern. Fortunately, this only requires exactly three operations: `(name==0 && body==0)`.


## Manifests

While PLAN is a Turing-complete evaluation model, we don’t just want to compute. We want to store data and perform actions in the real world. For this reason, we use PLAN to write a manifest that instructs a virtual machine how to do this. A manifest can be stored in many different but equivalent ways on disk, but in the end they all code for a PLAN expression that specifies something called a **cog**, which is responsible for keeping data and business logic intact and in sync. Among other things, a cog includes a set of **drivers**, which instruct the VM on how the cog needs to connect to the outside world. A driver could for example be a stateless HTTP server or an RTP video streamer.

Both cogs and drivers are persisted and evolving pieces of code that will resume their operation if the VM is ever rebooted, moved, or upgraded. The difference is that after a VM reboot, a cog will resume from its *latest* state, while a driver will resume from its *initial* state. The initial states of all drivers are included in the cog’s state; the cog tells the VM which drivers it requires in order to interact with the outside.

Both cogs and drivers are transition functions expressed in PLAN – closures that are ready to receive an input and will produce a new transition function as a result. Owing to their differences, cogs and drivers look slightly different from each other. We’ll start by looking at cogs.

### Cogs

::::warning[TODO]
Diagram forthcoming
::::

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

See the [cog reference](../reference/vm-and-interpreter/cogs.md) for a more detailed description of this process.

### Drivers

::::warning[TODO]
Diagram forthcoming
::::

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

See the [driver reference](../reference/vm-and-interpreter/drivers.md) for more information on the driver format.

#### *Effects*

::::warning[TODO]
Diagram forthcoming
::::

Drivers can ask the VM to do three things: query the cog’s current state, submit an input to the cog, and handle outside connections. They do this by emitting effects. Effects are simply arrays which contain the name of an effect together with some data. The runtime, or an in-system virtualization environment, will read this, perform some operation, and might inject an input back into the driver at a later time. 

For now, the only interface that workers have to the outside world is TCP. UDP will be added in the future. You can do basically everything over these – for example the host machine’s file system can be read and manipulated over TCP using an FTP driver. By limiting ourselves to proven network transports such as TCP and UDP, we can do everything within the system instead of having to continuously extend the runtime, thus maximizing forward compatibility and [making it easy to support new architectures](../philosophy/placeholder.md).

Of course, some things require lower latency than either TCP or UDP can offer, for example real-time audio or graphics. Eventually we’ll have to add effects for such interfaces, but we expect these to be few and new ones to be very infrequent. We can’t guarantee perfect forward compatibility, but we expect it to be very good in practice.

See the [effect reference](../reference/vm-and-interpreter/effects.md) for a list of all available effects.

### Jobs

When discussing the cog’s array of workers, we mentioned that a worker can be either a driver or a parallel evaluation **job**. While drivers handle concurrency, jobs handle parallelism. A job is simply a PLAN expression that the cog asks the VM to evaluate. Once evaluation has crashed or completed, the result will be logged and injected as an input to the cog.

See the [job reference](../reference/vm-and-interpreter/jobs.md) for more information.
