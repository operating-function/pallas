Plunder is a new programming model where programs run forever.

Hardware restarts are invisible to the software, as is moving a running
program from one physical machine to another.

Because of this, programs don't need to save their state into an external
database, they can simply keep everything "in memory", and that state
will be synchronized to disk transparently.

This is a prototype implementation of Plunder.

-   See `docs/` for information about the individual technologies: Rex,
    Sire, PLAN, Cogs, etc.

-   To run the sire tests, run `sh/sire-load-all`.

-   To run the Haskell tests, run `stack test`.

-   To run the `fulltag` demo, see the instructions in
    `sh/full-tag-site-demo`.

-   To build the `plunder` executable via nix, run `nix build .#plunder`
    (flakes-compatible nix required). Targets for `rex` and `plock` are
    also available.
