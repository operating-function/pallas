```ascii

                                    +---------------------+
                                    |           ┏         |
                                    |       ┏┓┏┓╋┏┓       |
                                    |       ┗┛┣┛┛╹┗       |
                                    +---------------------+

                                     an Operating Function
                          is a purely functional programming environment
                                 virtual machine for constructing
                               resilient, distributed applications.

```

https://github.com/user-attachments/assets/9c975b9b-5c17-4d41-8f9b-1015b8b81e28

_(Video) A basic web app demonstration leveraging an in-system HTTP server. All state is automatically persisted and computations can be paused and resumed:_ [demo](https://general-static-assets.nyc3.cdn.digitaloceanspaces.com/docs-images/notepad-demo.mp4)

---
---

- **Discover what an Operating Function is**: [opfn.co](https://opfn.co)
- **Understand why we care; and why you might, as well**: [Motivation](https://opfn.co/about)
- **Learn the system**: [Online Technical Documentation](https://docs.opfn.co) (or `/doc` directory here)

---
---

1. [Installation](#installation)
2. [Getting Started](#getting-started)
3. [Contributing](#contributing)
4. [Additional Resources](#additional-resources)


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
[the documentation](https://docs.opfn.co)

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

(For more on how Pallas machines work, see [the documentation](https://docs.opfn.co/explanation/vm-and-interpreter)).

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

# Additional Resources

- [Technical Documentation](https://docs.opfn.co)
- [Operating Function Company](https://opfn.co/)
- [Telegram Support](https://t.me/vaporwareNetwork)
