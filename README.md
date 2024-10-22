```ascii
    +---------------------------------------------------------+
    |                                                         |
    |     .+.                                                 |
    |    // \\                    __l  __l                    |
    |   //   \\                     l    l                    |
    |   \\   //                     l    l                    |
    |    \\ //              ____    l    l    ____    .--.|   |
    |     \Y/    _p pppp   /    a   l    l   /    a  (s       |
    |      U      p/    p   __aaa   l    l    __aaa   ssss.   |
    |    ==U==    pp    p aa   /|   l    l  aa   /|       s   |
    |      U      p \_pp   aaaa ./ _l_  _l_  aaaa ./ |\___/   |
    |             p                                           |
    |            _p_                                          |
    |                                                         |
    +---------------------------------------------------------+
                                                            ┏
                                                        ┏┓┏┓╋┏┓
                                                        ┗┛┣┛┛╹┗

         is a purely functional programming environment
                virtual machine for constructing
              resilient, distributed applications.
```

_(Video) A basic web app demonstration leveraging an in-system HTTP server. All state is automatically persisted and computations can be paused and resumed:_ ![demo](https://general-static-assets.nyc3.cdn.digitaloceanspaces.com/docs-images/notepad-demo.mp4)

# Index

1. [Motivation](#motivation)
2. [Stack Features](#)
4. [System Overview](#)
2. [Installation](#)
3. [Getting Started](#)
5. [Contributing](#)
6. [Caveats](#)
7. [Additional Resources](#)


# Motivation

The current [networked information system stack -- we need to arrive at a term for this. "Internet" for now] internet doesn't permit full inspection - it requires you to trust.  
This means you cannot truly own your computer or phone, nor the networks you interact with, nor the increasingly wide swaths of your life that are defined by being downstream of this complex. By "own", we mean: fully control, trust, change, opt-out, freely-share.

This trust problem exists up and down the modern information system stack: from the level of the programming language all the way up to the emergent societal patterns that arise from software-mediated, networked social interactions. The result is a digital ecosystem commons and society that is rigid, centralized, homogeneous, wasteful, interdependent, opaque. And software systems that are vulnerable to supply chain attacks, censorship and monopolization.

We want the opposite. We want **resilience**.

Resilient systems...

> ...adapt to changing conditions, long-term  
> ...have no single point of failure  
> ...are comprised of a diversity of components  
> ...are efficient with available resources  
> ...can be fragmented without catastrophic failure  
> ...are transparent and don't require trust in authority  

An internet of resilient computers...

> ...enables composable software, directly resulting in more software freedom
> ...empowers individual developers in a widely distributed manner
> ...cannot be censored nor monopolized
> ...insures that a user's computer is ubiquitous
> ...runs forever
> ...is robust against supply chain attacks 

# Stack Features

## Write once, run forever

- **Serialize anything, running programs included**
  - Closures can be serialized and stored on-disk, or sent over the wire. Programs in mid-execution can be paused, moved to a new machine, and resumed with no impact. Open syscalls are included in persisted state and are resumed on reboot.
- **Parallelism with deterministic replay**
  - Results from spawned processes, IO, and runtime evaluated expressions are recorded as events using an event-log-and-snapshot model. On replay, terminated events are recomputed with perfect determinism. 
- **Universal Portability**
  - Zero host-machine dependencies. Communicates with the outside world via a minimal set of pervasive interfaces (TCP, etc.). Runtime development is easy; thus, futurproof. System calls are formally specified as pure functions and their spec is designed to be frozen.

## All software is malleable

- **Human-Readable All the Way Down**
  - The underlying data model, PLAN, is human-readable (though barely human-writable), which means you don't need to _trust_ software or compiler binaries, if you'd prefer to audit them. **TK: needs a mention of bootstrapping and sire-in-sire**.
- **Universal data composability**
  - From an operator's point of view, all data is equally accessible and composable. "Application UIs" are merely projections, filters and actions applied to a single state. No app silos or walled gardens.

## Uncensorable content, Access to open markets

- **Global referentially-transparent content store**
  - Data and code is deduplicated, merkleized, and stored in content-addressable memory pages. This creates a global referentially-transparent content store, which is naturally complemented by protocols like BitTorrent.
- **Native networking and identity**
  - VMs and spawned processes are identified by one or more cryptographic keys. The networking protocol is stateless and guarantees at-least-once-delivery.
- **Peer-to-Peer permissionless distribution**
  - Connections with other operators are peer-to-peer. Sharing runnable (or running) code is no different than sharing content.
- **Infrastructure autonomy**
  - While the market may provide industrial-grade compute for your use if you choose it, falling back to an old laptop in your closet will always work if all else fails. Your freedom of speech is guaranteed by the system, but reach is your responsibility.

## Zero ongoing server costs, no DevOps

- **Users provide their infrastructure, not developers**
  - When users have a computer in the cloud, application developers don't have to bear the financial and legal liabilities of maintaining user infrastructure.
- **Extensible full-stack language platform**
  - Metaprogramming capabilities include hot reload, zero-overhead virtualization, macro-based type systems, all the way up to custom compilers. Both backend concerns and UIs are generated by the same language.
- **No database code**
  - All application data is automatically persisted, without the need for imports or boilerplate. To create a database, you write a pure transition function.

# System Overview

Pallas collapses the distinction between runtime, database, and operating system. The foundation of Pallas is untyped, but conceptually we can say that a Pallas process is a database of type:

```haskell
type DB = Input -> (Output, DB)
```

If a user supplies such a function, the Pallas runtime will create a database using a snapshot-and-event-log system. The user can write their programs as if they were keeping their data "in memory", without any need for manual persistence or other forms of cache management.

The recursive part of the type above might seem strange. You can think of it almost as a normal stateful function:

```haskell
type OtherDB = (State, Input) -> (State, Output)
```

The difference is that instead of changing the state value, the recursive version would change itself. The current version of the function is the state. In other words: programs can upgrade themselves dynamically. Code can construct code. Because of this, we can put an entire compiler toolchain inside the system and the programs it generates have zero dependencies on the outside world.

# Installation

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

## Install as a Development Environment

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

# Getting Started

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

# Contributing

At this stage in Pallas development, these are the types of contributions that
are most appropriate:

- Bugs in the existing examples
- New examples
- Documentation improvements
- Technical questions or requests for clarification

That said, we encourage you to dive even deeper and submit PRs beyond these
suggestions.

[CONTRIBUTING.md](https://github.com/operating-function/pallas/blob/master/CONTRIBUTING.md)

# Caveats

Pallas is still considered to be a prototype implementation, though some core features are close to done. It currently requires an underlying OS and file system, but has been designed such that these dependencies can eventually be removed.

Planned, but incomplete features:
- **Sire improvements** - _in progress_
   - Pallas supports macro-based type systems. There is a Hindley–Milner implementation that is ~80% complete that needs to be finished before serious applications are produced.
   - Automatic module linearization will significantly improve LLM integration and reduce onboarding challenges.
   - Add Sire macros for richer namespacing.
- **Capability-based process security model** - _in progress_
   - Cogs are designed to run hierarchically with a token, or capability-based, security model. The cog development model is under active development, but capabilities have not yet been added.
- **Native networking** - _not started_
   - Native networking is the least complete core feature. There are designs for a basic implementation using TCP, but more sophisticated approaches are possible.
- **Scalable runtime** - _not started_
   - The existing Haskell runtime is considered adequate for prototyping and hobbyists in the near term, but cannot scale to our required terabytes of data. The runtime will need to be rewritten in a systems language like C, Rust, or Zig. 


# Additional Resources

- [Technical Documentation](https://opfn.gitbook.io/pallas) 
- [Operating Function Company](https://blog.vaporware.network/)
- [Telegram Support](https://t.me/vaporwareNetwork)
