Sire
====

Sire is a lisp-like macro-assembler for PLAN.

Sire is the "seed" used to bootstrap plunder.  Sire is implemented
directly by the runtime system, and then the ecosystem bootstraps itself from scratch using the macro system.

This avoids the need for compilers to ship opaque binaries for
bootstrapping.

Sire has the following features:

-   Lambdas
-   Let bindings
-   Macros
-   Function inlining
-   Modules
-   Some trivial optimizations.

Lambda are implemented via straightforward lambda-lifting.

    x&(y&x)

        ->

    x&((x y & x) x)

The module system is very simple.  It is nothing more than a trivial
way to break programs up into multiple files.

Function inlining must be explicitly requested by the user, not by
heuristic optimizations.

Since Sire is designed to be a fully-specified language with deterministic
output, the optimizations will need to be fully specified.  But that's
TBD.  Here's a brief (and incomplete overview).

-   Single-use lets are eliminated.
-   Minimal plan-encoding of law bodies.
-   Etc.


Table of Runes
--------------

### Commands

-   `=` top-level binding.

    ```
    x=3                  ;;  Bind a value

    (f x)=x              ;;  Bind a pinned law

    f=(4 (f x ? x))      ;;  Same as the above

    "**"=tar_tar         ;;  Binds the `**` rune to a macro
    ```

-   `<` print entire closure.

    ```
    REPL> add
    (add a b)=(exec:3 toNat-a b)

    REPL> <add
    (exec a b c)=(2 b (exec a a-b) c)
    (toNat a)=(2:0:3 a)
    (add a b)=(exec:3 toNat-a b)
    ```

    This isn't so much a language features as a REPL feature.


-   `!!= x y` asserts that `x` equals `y`.

    ```
    !!= 2 (add 1 1)
    !!= add car-(add 1)
    ```

-   `/+` imports a module

    ```
    /+  str   [strCat strTake explode]
    /+  json
    ```

-   `^-^` filters the environment (all bindings not listed become unbound)

     This mainly is used to control the "export list" of a module.

     ```
     x=3
     y=4

     ^-^ x

     y ;; undefined
     ```

### Expressions

-   Variable references:

    ```
    x      ;; Variable "x"
    ."x"   ;; Variable "x" (useful to refer to runes and other bindings
           ;;               that have non-symbol names)
    ```

-   Nat literals (all of these are the same value)

    ```
    7303014
    0x6f6f66
    "foo"
    'foo'
    """foo
    ```

    `"string"` literals are encoded with utf8, and then converted to
    natural numbers via least-significant-byte encoding.

    ```
    !!= 'a'  0x61
    !!= 'z'  0x7a
    !!= 'az' 0x7a61
    ```

-   Function Application

    ```
    (f x y)    ;;  short-hand for (| f x y)
    (- f x y)  ;;  alternative syntax for (| f x y)
    f-x-y      ;;  Same as the above, but written infix.
    (| f x y)  ;;  Function application
    (| *f x y) ;;  Inlined Function application
    ```

-   Reordering Expressions

    `(^ _ x y)f` is a short-hand for:

    ```
    @ _ f
    | _ x y
    ```

    This is mostly useful for loops and things like that:

    ```
    ^ _ NIL args
    ? (loop acc remain)
    | if (null remain) acc
    | loop
      (CONS (listHead remain) acc)
      (listTail remain)
    ```

-   Function Inlining

    ```
    (**f x y)
    ```

    Here, if `f` is a top-level function binding that takes two arguments,
    then the funcion body will be inlined into the call-site.

    TODO: Specify exactly when inlining is accepted and exactly what
    it does.

-   Lambdas: `&` is anonymous, `?` is named, and `??` is named and pinned.

    ```
    (map (x & add 2 x) ~[3 4 5])

    ^ NIL args
    ? (loop acc remain)
    | if (null remain) acc
    | loop
      (&CONS (listHead remain) acc)
      (listTail remain)
    ```

    anonymous functions (`&` rune) will have a `lawName` of 0, and named
    functions (`?` rune) will have their name encoded in the underlying
    `lawName`.

-   `@` and `@@` do let bindings.

    Both recursive `@@` and non-recursive let bindings compile to the
    same PLAN, the only difference is with how names are resolved.

    ```
    @ x 3
    @ x (add 2 x)
    @ x (add 4 x)
    x
    ```

    ```
    @@ ones (1 ones)
     | eql 1 (car ones)
    ```

Macros
------

Macros are bound to runes, and they are functions that take rex trees
and return rex trees.

Macros are defined simply by binding a value to a rune-shapred variable.

    '###'=haxHaxHax

Macros can redefine the syntax of the base runes as well.

    `&`=lambda

This allows the base-syntax to be extended with macros.

Since these base-rune macros still need to expand to something, all of
the basic syntactic forms also work if prefixed with a #.

    = apply
    ? (x y)
    | apply x y

    #= apply
    #? (x y)
    #| apply x y

Macros typically expand to the stable '#'-prefixed runes.
