
<div align="center">
  <picture>
    <img alt="Pallas Logo" src="https://github.com/user-attachments/assets/2da906d9-bd3a-4473-b8e3-1a9b2ac427ae" width="60%">
  </picture>
</div>

<br />
<h1 align="center">An event sourced, purely functional application platform.</h1>

<div align="center">
Pallas makes it possible for small teams of developers to build high quality distributed applications. By using this technology, we aim to take back control of personal computing from big technology and make the internet more free.
</div>

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

Pallas is an event sourced, purely functional application platform, called an **_operating function_**. Every operation inside an operating function is ACID. Pallas is currently instantiated as a VM and can be run on Linux, Mac, or Nix. The platform ships with a minimal bootstrapping language called _Sire_, but includes an efficient axiomatic IR which can be targeted by mainstream functional languages.

### Problem
Software complexity has exploded. Our most popular applications require hundreds or thousands of developers to build and maintain. This complexity benefits large corporations who use their dominant capital positions to monopolize and rent seek. The root of this problem is anchored in the continued use of 20th century technologies that were never designed for distributed computing, and so require enormous resources to use.


### Solution
More composable software systems directly result in more software freedom. The more power individual developers wield, the more that power is distributed. This is true even if "scrolling through silos" is the highest state of computing.

To increase developer power and software freedom, your entire computer needs to be inspectable and understandable. It should be composable and made of highly generic, unopinionated, easily understood, reusabe building blocks. 

Pallas includes both a suitable foundation for the future of computing and an initial set of building blocks.

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
    - VMs and spawned processes are identified by one or more cryptographic keys. The networking protocol is stateless and guarantees at-least-once-delivery. The runtime implements the protocol, allowing transport details to evolve without breaking internal software.

- **Formally specified system calls**
    - Software breaks at boundaries, so syscalls are specified as pure functions and their spec is designed to be frozen. Runtimes are responsible for matching (_TODO: enforcing semantics?_)
  
- **Extensible language platform**
    - Metaprogramming capabilities include hot reload, zero-overhead virtualization, macro-based type systems, all the way up to custom compilers. 
    

## System Overview

TODO: diagram, major components

The foundation of Pallas is untyped, but conceptually we can say that a process is database is a function of the type 

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

Sire is the programming language of Pallas.

The source code for the counter cog will provide a brief look at Sire. The
[documentation](https://opfn.gitbook.io/pallas/sire/intro) covers the
language more fully, but we want you to get a sense of it now.

![Sire source code for simple "count up" cog](https://general-static-assets.nyc3.cdn.digitaloceanspaces.com/docs-images/pallas-example.png)

Open the `/sire/demo_count_up.sire` file yourself to see more elaborate comments explaining the syntax in detail.

Get into a live REPL with `pallas sire sire/prelude.sire` and refer to the Sire documentation for a full tour of the syntax.

### Demo Examples

_COMING SOON_: An `/examples` filled with more complex Sire procedures.

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

A Pallas machine consists of a set of persistent processes known as Cogs.  

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
left off. Later on you'll be invited to explore the source code of this
simple demo and you'll notice that there is no explicit saving or
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

Planned features not yet complete:
- Haskell and C runtimes
- Macro-based Hindley–Milner type system
- Native networking with cryptographic keys as network addresses 
- Standardized syscall hardware interface
- Capability-based process security model


## Additional Resources

- [Technical Documentation](https://opfn.gitbook.io/pallas) 
- [Operating Function Company](https://vaporware.network)
- [Telegram Support](https://t.me/vaporwareNetwork)

_This project is a fork of [Plunder](https://sr.ht/~plan/plunder/)._
