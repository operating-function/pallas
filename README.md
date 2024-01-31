The Blunder SSI is a degenerate fork of Plunder.

Blunder is a "plan to throw one away" system.

While Plunder is working towards a stable, freezable ABI that can
provide strong portability and long term support guarantees, Blunder is
a vale-tudo, free-for-all SSI playground.

Blunder is libertine. Blunder doesn't need to be maintainable or stable.
Blunder is a drunken slut. She'll gladly accept whatever patches you put
out, without any real consideration or review. Feel free to make a mess,
Blunder is for fun.

## Umm, okay. But what's an SSI?

"Solid State Interpreter" is a term coined by the repudiated reactionary
Curtis Yarvin to describe Urbit, his failed software project and
neglected technocult.

Basically your data lives in a big JSON file, and that JSON file
contains a python script.

You shove JSON into the python script and replace the JSON file
with the output.

You don't update the JSON file on every input, and instead just append
all of the JSON inputs to a file. Every once in a while, you update the
big JSON file and delete all the inputs.

Since the Python script lives in the JSON file, it can update itself.

Now replace JSON and Python with something much simpler, and freeze the
language so it doesn't break all the time.  That's an SSI.

Oh, and change some stuff so that it isn't slow.

Hopefully that made sense, but it probably didn't. Oh well.

### What's the simplified thing?

Uh.. okay fine.  I guess we can talk about this here.

The thing that replaces JSON+Pythong is PLAN.

It's really simple.  In Haskell you would write the type like this:

    data Plan
        = PIN Plan
        | LAW Nat Nat Plan
        | APP Plan Plan
        | NAT Nat

### What are nats?

A "nat" is just a natural number: 0, 1, 2, etc

### What are apps?

An "app" is a partially applied function, like `(add 3)`.

We can use this to build data-structures, but that's pretty unintutitive,
so I don't want to explain that yet.  Pretend you didn't hear that.

### What are pins?

A "pins" is something with the on-disk structure.

Whenever we replace a huge JSON file, it's really expensive.  Putting data
in a PIN tells the runtime something like "put this stuff in another
file".  That way, instead of rewritting the big PLAN file, you just
rewrite the pin files that are different from the last time.

### What are laws?

"law" are functions, they don't do a whole lot.

Laws "super-combinators", if that's something you want to read about.

Laws take a fixed number of arguments, and then produce an expression made
up only of function calls.  You can kinda think of them as "templates":
there are no nested functions or variables.

    const(x,y) => x

    doublePlusThree(x) => plus(mul(x,2), 3)

Okay, I lied.  You can have variables, but you can only define one block
of variables at the beginning, and you can't update them.

    sillyExample(x) =>
        letrec
            dub        = mul(2, x)
            dubPlusTwo = add(2, dub)
        in
            add(dub, dubPlusTwo)

But that's all.  You can't do anything else.

This stuff about functions makes no sense because I also said that the
PLAN datatype has only `| LAW Nat Nat Plan` and that has nothing to do
with anything that I just said.

Don't worry about that.  These functions are crammed into that structure
somehow, and the details aren't important rn.  It's not complicated,
but it is a little bit confusing.

### What's in the standard library?  What built-in functions are there?

There are only 5 built-in operations in the language, and they are things
like increment, decrement, isZero, "make law", and "make pin".

So, it's pretty hard to program directly in this system.

### How do you use this, then?

There's also a language call `Sire` which compiles to PLAN.  Sire supports
the full lambda calculus and has a lisp-like macro system, which is used
to add new features.

Sire is super tiny, because it is also frozen.  You can build your own
Sire compiler in just a few thousand loc.

Plunder programs are in Sire for now, but eventually Sire will just
be used to bootstrap a nicer language, and programs will be written in
that instead.

This lets you bootstrap a system entirely from source.

### How can you do anything interesting with just increment/decrement/etc?

Certain functions are recognized by the runtime system as "special".

For example, the "special" `add` function is something like:

    (_Add x y)=(exec inc (toNat x) y)

When the runtime system sees this function, is says "Oh, I know this one.
This is the add function", and it optimizes it with a custom internal
routine.

There is a standard set of these "builtins", and that set can grow
over time.

Builtins use this ugly naming convention `_Builtin`, just to make them
stand out.

This all probably seems kinda strange and pointless, but remember that we
can't change the file-format without breaking your big PLAN files.

This system of "special functions" makes it possible to add new "built-in"
functions without changing the formal definition of the system, and
without changing the file-format.

### Whis is this syntax you just used, you didn't talk about that?

Oh, this:

    (_Add x y)=(exec inc (toNat x) y)

That just means:

    <{_Add 2 (0 (0 (exec inc) (0 toNat 1)) 2)}>

Which means:

    (PIN
      (LAW 1684291935 2
       (APP (APP 0
             (APP
              (APP 0 (APP exec inc))
              (APP (APP 0 toNat) 1)))
         2)

But yeah, uh, we really don't need to get into that right now.

Again, it's not complicated, but it's very unintuitive.

People always get hung up on this, and the only real way to explain this
is to sit down and work through some examples until it clicks.


## Building Blunder

### Using Stack Without Nix

If you have `stack` (the Haskell build tool) installed, you can just
run:

    $ stack install

You may also need to install `lmdb` and `zlib` in the host os.

### Using Nix to Setup a Dev Environment

If you have a flakes-enabled `nix` in your PATH, you can use
`nix develop` to automatically enter a development-shell that contains
all of the relevant tooling.

    $ nix develop
    [<...>]$ stack build
    [<...>]$ bash sh/mandelbrot-ui-demo

### Building with Nix (Slow)

Similarly, if you have a flakes-enabled `nix` in your PATH, you do a
fully-reproducible build of Blunde like so:

    $ nix build .#plunder

The current build dependencies are not in the normal nix caches, so this
approach will take up a lot of time and space.

This requires building 1 or more GHCs, which can be rather intensive.
There is also risk that, once built, the GHCs may be nix-GC-ed, thus
requiring rebuilds. For this reason, we expose a build target for
`haskell.nix` build environment. This can be combined with nix profiles
to ensure persistence:

    $ nix build .#hnix-roots --profile ./hnix-roots
