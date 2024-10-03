
<div align="center">
  <picture>
    <img alt="Pallas Logo" src="https://github.com/user-attachments/assets/fee06d57-ae41-4238-b35e-b5edb384a161" width="70%">
  </picture>
</div>

<br />
<h1 align="center">An event sourced, purely functional application platform.</h1>

<div align="center">
Pallas is a backend platform that makes it easy to build and run p2p, distributed applications. It's like a Turing complete social network where messages can also be programs.</div>

<br/>

## Index

1. [Introduction](#introduction)
2. [Features](#features)
2. [System Overview](#installation)
4. [Example](#example)
2. [Installation](#installation)
3. [Getting Started](#getting-started)
5. [Contributing](#contributing)
6. [Caveats](#caveats)
7. [Additional Resources](#additional-resources)


## Introduction

Pallas is an event sourced, purely functional application platform, called an **_operating function_**. Every operation inside an operating function is ACID. Pallas communicates with the outside world through a small set of frozen hardware interfaces and manages internal processes through an Erlang-style actor model.

The platform ships with a minimal bootstrapping language called _Sire_, but includes an efficient axiomatic IR which can be targeted by mainstream functional languages.

### Problem
_Software is being destroyed by accidental complexity_. Our most popular applications require hundreds or thousands of developers to build and maintain. This complexity benefits large corporations who use their dominant capital positions to monopolize and rent seek. This dynamic increases the cost of software, reduces the set of economically viable programs, and disempowers developers and users alike.

This is not an abstract moral problem. Software is the best tool we have for identifying, organizing, and solving societal issues. Instability in software trickles down to every other domain of human activity.

### Solution
*More composable software systems directly result in more software freedom.* The more power individual developers wield, the more that power is widely distributed. This is true even if "scrolling through silos" is the highest state of computing.

To increase developer power and software freedom, your entire computer needs to be inspectable and understandable. It should be composable and made of highly generic, unopinionated, easily understood, reusable building blocks. These properties need to be stable regardless of underlying hardware changes and should guarantee a high degree of backward and forward compatibility.


## Features

- **No database code**
   - All application data is automatically persisted, without the need for imports or boilerplate. To create a database, you write a pure function.

- **Serialize anything, running programs included**
    - Closures can be serialized and stored on-disk, or sent over the wire. Programs in mid-execution can be paused, moved to a new machine, and resumed with no impact. Open syscalls are included in persisted state and are resumed on reboot.

- **Parallelism with deterministic replay**
    - Results from spawned processes, IO, and runtime evaluated expressions are recorded as events using an event-log-and-snapshot model. On replay, terminated events are recomputed with perfect determinism. 

- **Global referentially-transparent content store**
    - Data and code is deduplicated, merkleized, and stored in content-addressable memory pages. This creates a global referentially-transparent content store, which is naturally complemented by protocols like BitTorrent.

- **Native networking and identity**
    - VMs and spawned processes are identified by one or more cryptographic keys. The networking protocol is stateless and guarantees at-least-once-delivery.

- **Formally specified system calls**
    - Software breaks at boundaries, so syscalls are specified as pure functions and their spec is designed to be frozen. As long as the spec is satisfied, runtimes can change implementations without impacting internal software.
  
- **Extensible language platform**
    - Metaprogramming capabilities include hot reload, zero-overhead virtualization, macro-based type systems, all the way up to custom compilers. 
    

## System Overview

Pallas collapses the distinction between runtime, database, and operating system. The foundation of Pallas is untyped, but conceptually we can say that a system process is database with a type of:

```haskell
type DB = Input -> (Output, DB)
```

If a user supplies such a function, the Pallas runtime will create a database using a snapshot-and-event-log system. The user can write their programs as if they were keeping their data "in memory", without any need for manual persistence or other forms of cache management.

The recursive part of the type above might seem strange. You can think of it almost as a normal stateful function:

```haskell
type OtherDB = (State, Input) -> (State, Output)
```

The difference is that instead of changing the state value, the recursive version would change itself. The current version of the function is the state. In other words: programs can upgrade themselves dynamically. Code can construct code. Because of this, we can put an entire compiler toolchain inside the system and the programs it generates have zero dependencies on the outside world.

## Example

**Sire** is the default programming language of Pallas. A Pallas machine consists of a set of persistent processes known as Cogs.  

This code is for a simple counter cog that gets the time and increments a value. Counter state is persisted through cog restarts.

The [documentation](https://opfn.gitbook.io/pallas/sire/intro) covers the language more fully.

```
#### demo_count_up <- prelude

:| prelude

;;; Count Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

= (countLoop count k)
| trk [{counter is at} count]
| syscall TIME_WHEN
& now
: resultOfTimeWait < syscall (TIME_WAIT (inc now))
| countLoop (inc count) k

= (main)
| runCog (countLoop 0)

;;; Example output:
;;; ++ [%trk {2024-08-02T17:05:48.468929096Z}]
;;; ++ [{counter is at} 1]
;;;
;;; ++ [%trk {2024-08-02T17:05:49.470040565Z}]
;;; ++ [{counter is at} 2]
;;;
;;; ++ [%trk {2024-08-02T17:05:50.471186301Z}]
;;; ++ [{counter is at} 3]
```

### Explanation

```
#### demo_count_up <- prelude
```

Inline 'prelude' before 'demo_count_up'.

`:| prelude`

Import 'prelude.sire'.

---

```
= (countLoop count k)
```

`=` declares a top level binding. The function named `countLoop` takes a 'count' parameter (and a 'k' parameter that we don't need to worry about now).

---

```
| trk [{counter is at} count]
```

Print out the current count to the console with the `trk` (track) function from the standard library. The bar `|` is function application. It says "apply the function `trk` to the arguments after it."

`trk` is a function that takes two arguments: 
- the message to log to the terminal and 
- a value that it will simply return (often the rest of the program)

---

```
| syscall TIME_WHEN
& now
```

Next, we want to do a clock system call for the current time. The `syscall` function takes two arguments:
 - a request type of `TIME_WHEN` ("give me the current system time" in this case).
 - a continuation to use when the syscall completes.

 The system call might take a while to complete, but when it does, we'll be woken up and the result of the call will be passed as an argument to the continuation function that was provided to `syscall`. We use `&` to define the continuation function as a lambda. The lambda takes one argument called `now`, which will be bound to the result of the `TIME_WHEN` call.

 Pallas syscalls are not like those found in GNU C. Each syscall and response is mapped in an array managed by a small internal micro-kernel. The Pallas runtime is responsible for actually interacting with the external system.

 ---

```
: resultOfTimeWait < syscall (TIME_WAIT (inc now))
```
 
 Now we want to wait 1 second. We'll use this opportunity to show an alternative style that you'll come across often in Pallas code.

 We're going to use the `TIME_WAIT` syscall. `TIME_WAIT` itself takes a single argument - the amount of time to wait. We want to wait 1 second, which is the current time plus 1. At this point we are in the body of the continuation and have `now` in scope. The `inc` function takes a value and returns the result of adding 1 to it. `inc` is applied to `now` by wrapping both in parentheses.

 Also note that rather than using the `&` anonymous lambda style, we're now using the col macro, `:` ("col" as in colon).

On the right side of the `<` we're doing the syscall and the result of that call gets bound to `resultOfTimeWait`. As with the previous syscall, the next argument is a continuation, which again, is the rest of the code below the col macro.

The col macro is a method of writing continuation-passing style in a way that *feels* like assignment. It feels as if syscall returns `resultOfTimeWait` which can be used in the remainder of the body. Col macro-expands into the same code as the `&` version above.

Our goal with `TIME_WAIT` was just to wait 1 second. We don't actually use the "result" of the `TIME_WAIT` syscall (bound to `resultOfTimeWait`). In this case, we could also have bound it to "_" to denote this.

---

```
| countLoop (inc count) k
```

After waiting 1 second, we recurr and pass an incremented value for `count`.

---

```
= (main)
| runCog (countLoop 0)
```

Finally, we'll start a process that will be responsible for running this function and handling any threads or syscalls that are involved. These processes are called "cogs" and are initiated with the `runCog` function.

We bind a top-level `main` function that will call `runCog`. When we pass `countLoop` to `runCog` as its "job", we also need to provide the starting count of zero.



## Installation

You have two options: build a dev environment using Nix, or clone the repo and install a binary. The binary is supplied for convenience only and still requires the source code in order to function.

### Install with Binary (Ubuntu/Debian-based and MacOS)

1. Install dependencies:
   - libgmp (GNU Multiple Precision Arithmetic Library)
   - liblmdb (Lightning Memory-Mapped Database)
   - libz (zlib compression library)

On Ubuntu or Debian-based systems:

```console
sudo apt-get update && sudo apt-get install -y \
    libgmp10 \
    liblmdb0 \
    zlib1g
```

On MacOS:

```console
brew install gmp lmdb zlib
```

2. Download a prebuilt binary
   - [Linux x86_64](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/linux_x86_64/pallas) 
   - [Apple arm64/aarch64 (M1/M2 Macs)](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_m1_arm64/pallas)
   - [Apple x86_64 (Intel Macs)](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_x86_64/pallas)

Your browser may not prompt to download these files, in which case you can use `cURL`:

`curl -L <URL of your choice here> -o pallas`

Make it executable and move it somewhere on your path.

3. Run it:

If all went well, you should see this:

```console
$ pallas

Run a Pallas machine

Usage: pallas COMMAND

  Pallas

Available options:
  -h,--help                Show this help text

Available commands:
  sire                     Run a standalone Sire repl.
  save                     Load a sire file and save a seed.
  show                     Print a seed file.
  repl                     Interact with a seed file.
  start                    Resume an idle machine.
  boot                     Boot a machine.
```

### Install as a Development Environment

Using Nix is the most straightforward way to install Pallas at this time. 
If your system doesn't support Nix or if you need further
instruction (including instructions for Docker), refer to
[the documentation](https://opfn.gitbook.io/pallas/installation/installation).

1. Clone this repo. Navigate to the root of it.

```shell
git clone git@github.com:operating-function/pallas.git
```
```shell
cd pallas
```

2. Get into a Nix shell

```shell
nix develop
```

3. Build pallas

This will take some time. Perhaps upwards of 15 minutes, depending on your system.

```shell
stack build
```

4. Confirm everything is working:

```console
$ stack run pallas

Run a Pallas machine

Usage: pallas COMMAND

  Pallas

Available options:
  -h,--help                Show this help text

Available commands:
  sire                     Run a standalone Sire repl.
  save                     Load a sire file and save a seed.
  show                     Print a seed file.
  repl                     Interact with a seed file.
  start                    Resume an idle machine.
  boot                     Boot a machine.
```

## Getting Started

Navigate to the root of this repository and run the commands below to see
a simple demonstration of running a Pallas machine.

(The following demo uses `/tmp` as the location on the host filesystem
to create a directory named `counter` to hold the event log of the machine.
Feel free to use a different directory if you'd like.)

```console
pallas boot /tmp/counter sire/demo_count_up.sire
pallas start /tmp/counter
```

Take note of the final counter value and then `Ctrl-C` to kill the machine.

```console
++ [%trk {2020-09-21T10:15:00.729241178Z}]
++ [{counter is at}=7]
```

Now run `pallas start /tmp/counter` again. The counter picks up where it
left off. You'll notice that there is no explicit saving or
writing to disk or a database. You get persistence for free by writing
application code.

(For more on how Pallas machines work, see [the documentation](https://opfn.gitbook.io/pallas/overview/overview#ships)).

## Contributing

At this stage in Pallas development, these are the types of contributions that
are most appropriate:

- Bugs in the existing examples
- New examples
- Documentation improvements
- Technical questions or requests for clarification

That said, we encourage you to dive even deeper and submit PRs beyond these
suggestions.

[CONTRIBUTING.md](https://github.com/operating-function/pallas/blob/master/CONTRIBUTING.md)

## Caveats

Pallas is still considered to be a prototype implementation, though some core features are close to done. It currently requires an underlying OS and file system, but has been designed such that these dependencies can eventually be removed.

Planned, but incomplete features:
- **Sire improvements** - <span style="color:orange">in progress</span>
   - Pallas supports macro-based type systems. There is a Hindley–Milner implementation that is ~80% complete that needs to be finished before serious applications are produced.
   - Automatic module linearization will significantly improve LLM integration and reduce onboarding challenges.
   - Add Sire macros for richer namespacing.
- **Capability-based process security model** - <span style="color:orange">in progress</span>
   - Cogs are designed to run hierarchically with a token, or capability-based, security model. The cog development model is under active development, but capabilities have not yet been added.
- **Native networking** - <span style="color:red">not started</span>
   - Native networking is the least complete core feature. There are designs for a basic implementation using HTTP, but more sophisticated approaches are possible.
- **Scalable runtime** - <span style="color:red">not started</span>
   - The existing Haskell runtime is considered adequate for prototyping and hobbyists in the near term, but cannot scale to our required terabytes of data. The runtime will need to be rewritten in a systems language like C, Rust, or Zig. 


## Additional Resources

- [Technical Documentation](https://opfn.gitbook.io/pallas) 
- [Operating Function Company](https://blog.vaporware.network/)
- [Telegram Support](https://t.me/vaporwareNetwork)

_This project is a fork of [Plunder](https://sr.ht/~plan/plunder/)._
