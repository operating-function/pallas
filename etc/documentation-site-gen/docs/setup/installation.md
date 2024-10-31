# Environment Setup

## Docker quickstart

If you're interested in the quickest way to get a Sire REPL and start playing with the language and you have Docker on your system, you can simply pull the Pallas image [found here](https://github.com/deathtothecorporation/pallas-docker). That repo has [instructions](https://github.com/deathtothecorporation/pallas-docker) for quickly getting straight into a Sire REPL.

Once you're in the REPL, you should see something this:

```
<...>
("20_prp","LOADED FROM CACHE!")
("21_par","LOADED FROM CACHE!")
("boot","LOADED FROM CACHE!")

}
} ==== Sire REPL ====
}
} Since input is multi-line, there is currently no input-prompt.
} Just type away!
}

```

Try entering `(add 1 2)` and hitting enter.

Let's [learn some more Sire now](/sire/intro.md)

## Prebuilt binary

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

2. Get a prebuilt binary:

Currently we provide the following prebuilt binaries:
- Linux x86_64: [https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/linux_x86_64/pallas](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/linux_x86_64/pallas)
- Mac arm64/aarch64 (M1 macs): [https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_m1_arm64/pallas](https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/apple_m1_arm64/pallas)

Your browser may not prompt to download these files, in which case you can use `cURL`:

`curl -L <URL of your choice here> -o pallas`

Make it executable and move it somewhere on your path.

3. Run the `pallas` binary to confirm all is well:

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

4. Get a Sire REPL:

Clone [the Pallas repository](https://github.com/operating-function/pallas) and navigate to its root. Then run:

```console
$ pallas sire sire/prelude.sire
...
...
("prelude","LOADED FROM CACHE!")

}
} ==== Sire REPL ====
}
} Since input is multi-line, there is currently no input-prompt.
} Just type away!
}

```

(Ctrl-C to get out of the REPL)


## Install from source

First off, you'll need the Pallas source code. Clone it from [https://github.com/operating-function/pallas](https://github.com/operating-function/pallas). The rest of this guide will assume you're at the root of that repo.

### Dependency installation and first-run

> If your system does not or cannot run `nix`, you would need to install the Haskell and its build tool, `stack`, as well as `lmdb` and `zlib`. Once these requirements are satisfied, you can `stack install` at the root of the Pallas repo.

Because the current runtime is somewhat experimental, enormous time has not been invested into portability just yet. As a result, using `nix` is the most straightforward way to get Pallas running. (installing and configuring `nix` is outside the scope of this guide. We recommend using Docker if you're unable to use `nix`)

Any time you open a new terminal, you must remember to enter the development shell with nix by running:

```bash
nix develop
```

You will forget to do this and wonder why nothing is working. That's probably why.

Once you're in a nix development shell, run the following command to build the Pallas environment:

```bash
stack build
```

This might take a while! When it's done, you should be able to verify all went well by running the mandelbrot web app demo:

```bash
bash sh/mandelbrot-ui-demo
```

That command should output something like this:

```

<...A lot more stuff up here...>

tabSplitLT          MATCHED
    , e "tabSplitLT"          "UdzPypcfJLtyMjLy87ukmU7nRHtfEv3nnZbfJ34isWM"
tabSing             MATCHED
    , e "tabSing"             "7AfouhmiExP9KSySjrw4k4VZ5JY2TE4cW53xo1EhHsfz"
[{2024-01-12T16:22:20.897758578Z} {REPLAY TIME: 0 ns}]

HTTP_SPINNING
_http_port=56191


= _http_port_file
} /home/your-user/pallas/.demo/4172716296342791002.http.port


Setting interface files...
Interface files are set. Running on http://localhost:56191/index.html
```

There is now a webserver running and you can access the app at the URL mentioned in the final line of output. **NOTE: the `index.html` at the end is required!**

```
there was a mandelbrot image here
```

Change the width and height to 100 and click "resize" (Keeping a small size for expediency). Then click "generate fractal". After a few seconds you'll see a fractal appear in the canvas. Congratulations, you just ran a Pallas app.

Soon you'll learn how to write one of your own.

## The `pallas` command and REPLs

Enter this command in a shell at the root level of the pallas repo (where you've remembered to first run `nix develop`):

```
pallas
```

As of the time of this writing, you'll see the usage hint for the `pallas` command:

```
new-network - a test for running pallas machines

Usage: pallas COMMAND

  Let's run pallas.

Available options:
  -h,--help                Show this help text

Available commands:
  term                     Connect to the terminal of a cog.
  open                     Open a terminal's GUI interface.
  sire                     Run a standalone Sire repl.
  save                     Load a sire file and save a seed.
  show                     Print a seed file.
  repl                     Interact with a seed file.
  start                    Resume an idle machine.
  loot                     Run a standalone sire repl.
  boot                     Boot a machine.
  du                       du -ab compatible output for pin state.

```

We'll get into booting machines and running cogs soon, but first let's just get a Sire REPL to play with.\
While the `repl` command looks attractive, it's not going to do exactly what you expect.

There is a particular boot sequence required before we have all the necessary tools at hand. Since we want our environment to be hydrated properly, we'll execute the proper boot sequence in order to get a working REPL:

```
pallas sire sire/prelude.sire
```

You'll see something like this:

```
<...>
("20_prp","LOADED FROM CACHE!")
("21_par","LOADED FROM CACHE!")
("boot","LOADED FROM CACHE!")

}
} ==== Sire REPL ====
}
} Since input is multi-line, there is currently no input-prompt.
} Just type away!
}

```

Try entering `(add 1 2)` and hitting enter.
