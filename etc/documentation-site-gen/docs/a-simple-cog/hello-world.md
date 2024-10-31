---
description: 'Document Type: Tutorial'
---

# "Hello World" Cog

## "Hello World" Cog

You're finally going to start writing code in `.sire` files that you boot and run, rather than using the REPL. A few things to note: you can't have blank new lines within the body of a function, but comments (lines that start with `;`) are fine. Indentation matters, but you'll start to see that in practice - we won't explain it up front.

**Hello world!**

```sire
#### hello_world_cog <- prelude

:|  prelude

;;;;;

= (helloWorld return)
| trk "hello world"
| return ()

main=(runCog helloWorld)
```

## Running the Cog

Before we inspect the code, let's run it and see something happen!

First, create the file `sire/hello_world_cog.sire` (within the pallas repo, right next to all the other `sire/*.sire` files) with the above contents and save it.

Now, choose a directory where you want your pallas ships/VMs to get created. We'll use a `my_ships` directory for the examples here. We'll proclaim that the name of the ship (the directory name) that contains this cog will be `hello_world`. Thus, the command to boot this ship and initiate this cog is (you remembered to `nix develop` right?):

```
pallas boot ~/my_ships/hello_world sire/hello_world_cog.sire
```

When you run this command, you'll see a whole bunch of output, with something like this near the end:

```
<...>
("datom","LOADED FROM CACHE!")
("prelude","LOADED FROM CACHE!")
("hello_world_cog","LOADED FROM CACHE!")
(helloWorld a)=(_Trace:{hello world} a-0)

{hello world} ;; <-- Here's your trk-logged message

main=(KERNEL [0 0] 0 [0] [0])

("cache hash","7y5a286DrMFXWwSJBiPAXYnpoYjqEfJcvSJuiBw1GKV4")
```

Congratulations, you've booted a pallas ship that runs a single cog which logs a string to the terminal.

## Understanding the Cog

Now let's go through it line-by-line to get a high-level understanding.

### Imports

We don't want to get too far into the weeds just yet with includes, boot order and library imports, but we also don't want you to be puzzled by too many unexplained lines of code.

```sire
#### hello_world_cog <- prelude
```

This has to do with load order. `hello_world_cog.sire` is the name of the current file, and `prelude.sire` is the name of its single allowed dependency. Sire files can only have 0 or 1 dependencies. If a file has 0 dependencies, then it only has access to raw PLAN opcodes. If it has 1 dependency, then it can use anything that was defined by its transitive chain of dependencies. The Sire interpreter is a very simple state machine which simply loads definitions in order and changes its state after each one, so the `#### foo <- bar` syntax is saying that we should load `bar.sire` before `foo.sire` (and the same goes for any dependency of `bar.sire`, so the Sire interpreter always starts with a file with 0 dependencies).

```sire
:|  prelude
```

The `:|` rune imports a module. Whenever we declare load order using `#### foo <- bar`, we also enter a new namespace named `foo`, which any dependents need to explicitly import if they want to use definitions from it. In this case, `prelude.sire` first bootstraps Sire and then does a bunch of other importing of kernel code, convenience functions, syscalls, and basically everything else we need to run a cog.

```sire
;;;;;
```

This is just a comment line dividing the imports from the business code. lol.

### Binding the function

```sire
= (helloWorld return)
```

`=` defines a top-level binding for a function named `helloWorld` which takes a single argument named `return`.

This function only does a couple things:

```sire
| trk "hello world"
```

It applies the `trk` function (which you'll recall logs to the console) to the argument `"hello world"`, a string.

```sire
| return ()
```

And it applies the `return` argument that it received, sort of like a callback. Don't worry too much about this bit for now.

### `main` and `runCog`

Finally, we have this line:

```
main=(runCog helloWorld)
```

Think of this as boilerplate. Every cog should end in a `main=(runCog someNameHere)` line which can be thought of as ultimately kicking off the `someNameHere` process previously-defined.
