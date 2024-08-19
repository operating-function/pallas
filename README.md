# Pallas

An event sourced, purely functional operating system.

## Index

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Getting Started](#getting-started)
4. [Example](#example)
5. [Contributing](#contributing)
6. [Caveats](#caveats)
7. [Additional Resources](#additional-resources)

## Introduction

[![Webserver Demo](https://general-static-assets.nyc3.cdn.digitaloceanspaces.com/docs-images/pallas-persisted-webservers.mov)](https://general-static-assets.nyc3.cdn.digitaloceanspaces.com/docs-images/pallas-persisted-webservers.mov)

Pallas is an event sourced, purely functional operating system, called an operating function. 

Pallas provides an entirely unique set of features, but is inspired by a long history of systems and language research, including MIT’s exokernel. Pallas collapses the distinction between database, virtual machine, and language platform, creating a stable, simple, and extensible programming environment.

The cost to this design is a clean break with legacy systems, including Unix and most popular programming languages. In return, Pallas offers a set of features that are found nowhere else:
- All application data is automatically persisted, without the need for imports or boilerplate. To create a database, you write a pure function. Open system calls are included in persisted state, and so are resumed on reboot.
- Partially applied functions can be serialized and stored on-disk, or sent over the wire. Programs in mid-execution can be paused, moved to a new machine, and resumed with no impact.
- Program execution can be parallelized via a process model. A single machine can spawn and manage multiple processes.
- The system’s default language blends features from both Lisp and Haskell, with a syntax that is more readable and flexible than S-expressions without sacrificing regularity or homoiconicity. Metaprogramming capabilities include hot reload, zero-overhead virtualization, macro-based type systems, all the way up to custom compilers.
- Data and code is deduplicated, merkleized, and stored in content-addressable memory pages. This creates a global referentially-transparent content store, which is naturally complemented by protocols like BitTorrent.

The foundation of Pallas is untyped, but conceptually we can say that a database is a function of the type 

```haskell
type DB = Input -> (Output, DB)
```

If a user supplies such a function, the Pallas runtime will create a database using a snapshot-and-event-log system. The user can write their programs as if they were keeping their data "in memory", without any need for manual persistence or other forms of cache management.

The recursive part of the type above might seem strange. You can think of it almost as a normal stateful function:

```haskell
type OtherDB = (State, Input) -> (State, Output)
```

The difference is that instead of changing the state value, the recursive version would change itself. The current version of the function is the state. In other words: programs can upgrade themselves dynamically. Code can construct code. Because of this, we can put an entire compiler toolchain inside the system and the programs it generates have zero dependencies on the outside world.

This project is a fork of [Plunder](https://sr.ht/~plan/plunder/).

## Installation

### Quick Start (Ubuntu/Debian-based and MacOS)

The quickest way to run Pallas is by installing a few dependencies and grabbing
a pre-built binary.

1. Install dependencies:

Dependencies:
- libgmp (GNU Multiple Precision Arithmetic Library)
- liblmdb (Lightning Memory-Mapped Database)
- libz (zlib compression library)

On Ubuntu or Debian-based systems, you can install these with:

```console
sudo apt-get update && sudo apt-get install -y \
    libgmp10 \
    liblmdb0 \
    zlib1g
```

On MacOS, [Homebrew](https://brew.sh/) is a good option (assumes you have Homebrew installed):

```console
brew install gmp lmdb zlib
```

2. Get a pre-built binary:

Currently we provide the following prebuilt binaries:
- Linux x86_64: [https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/linux_x86_64/pallas](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/linux_x86_64/pallas)
- Apple arm64/aarch64 (M1/M2 Macs): [https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_m1_arm64/pallas](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_m1_arm64/pallas)
- Apple x86_64 (Intel Macs): https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_x86_64/pallas

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

### Get a Development Environment

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

## Example

Sire is the programming language of Pallas.

The source code for the counter cog will provide a brief look at Sire. The
[documentation](https://opfn.gitbook.io/pallas/sire/intro) covers the
language more fully, but we want you to get a sense of it now.

![Sire source code for simple "count up" cog](https://general-static-assets.nyc3.cdn.digitaloceanspaces.com/docs-images/pallas-example.png)

Open the /sire/demo_count_up.sire file yourself to see more elaborate comments explaining the syntax in detail.

Get into a live REPL with pallas sire sire/prelude.sire and refer to the Sire documentation for a full tour of the syntax.

### Demo Examples

_COMING SOON_: An `/examples` filled with more complex Sire procedures.

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

- Pallas is developed by [The Operating Function Company](https://vaporware.network)
- [Technical Documentation](https://opfn.gitbook.io/pallas)
  - or view the docs source files at the `doc/` git submodule.
- [OPFN Telegram](https://t.me/vaporwareNetwork)
