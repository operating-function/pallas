Plunder is a new programming model where programs run forever.

Hardware restarts are invisible to the software, as is moving a running
program from one physical machine to another.

Because of this, programs don't need to save their state into an
external database, they can simply keep everything "in memory", and that
state will be synchronized to disk transparently.

This is a prototype implementation of Plunder.

-   See `doc/` for information about the individual technologies: Rex,
    Sire, PLAN, Cogs, etc.

-   To run the sire tests, run `sh/sire-load-all`.

-   To run the Haskell tests, run `stack test`.

-   To run the `fulltag` demo, see the instructions in
    `sh/full-tag-site-demo`.


## Getting Connected

Matrix Chat:

-   [#plunderchat](https://matrix.to/#/#plunderchat:matrix.org)


Mailing Lists:

-   [plunder-discuss](https://lists.sr.ht/~plan/plunder-discuss)
-   [plunder-devel](https://lists.sr.ht/~plan/plunder-devel)
-   [plunder-announce](https://lists.sr.ht/~plan/plunder-announce)


## Building Plunder

### Using Stack Without Nix

If you have `stack` (the Haskell build tool) installed, you can just run:

    $ stack install

You may also need to install `lmdb` and `zlib` in the host os.


### Using Nix to Setup a Dev Environment

If you have a flakes-enabled `nix` in your PATH, you can use `nix develop`
to automatically enter a development-shell that contains all of the
relevant tooling.

    $ nix develop
    [<...>]$ stack build
    [<...>]$ bash sh/mandelbrot-ui-demo

### Building with Nix (Slow)

Similarly, if you have a flakes-enabled `nix` in your PATH, you do a
fully-reproduable build of plunder like so:

    $ nix build .#plunder

The current build dependecies are not in the normal nix caches, so this
approach will take up a lot of time and space.

This requires building 1 or more GHCs, which can be rather
intensive. There is also risk that, once built, the GHCs may be
nix-GC-ed, thus requiring rebuilds. For this reason, we expose a
build target for `haskell.nix` build environment. This can be
combined with nix profiles to ensure persistence:

    $ nix build .#hnix-roots --profile ./hnix-roots
