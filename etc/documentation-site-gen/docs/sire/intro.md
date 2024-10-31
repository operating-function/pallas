# Intro

## Intro to Sire

As already discussed, the _point_ of Sire is to get back PLAN. It is also possible to compile mainstream functional languages to PLAN. At that point, much Pallas development—including full-stack web applications—will be accomplished by writing in those languages only, leaving Sire only responsible for code that needs to be optimized by the runtime (like new crypto libraries, say).

Until that time, we'll be writing Sire. But since our immediate goal for the moment is to build a cog that runs and does some stuff, we're not going to learn every character and nuance of Sire syntax. Just enough.  
Don't worry if you don't fully understand _how_ a line of code does what it does, but do make sure you have a handle on _what_ it's doing.

## Getting familiar

Open up a bootstrapped Sire REPL. As a reminder:

```
nix develop
pallas sire sire/prelude.sire
```

Or if you're using the docker images:

```
docker pull deathtothecorporation/sire-repl
docker run -it deathtothecorporation/sire-repl
```

We're going to hands-on learn a few basic concepts: Top-level bindings, let bindings and function application.

A few meta-points to note at the outset:

* `;` is a comment in Sire. used here, it'll be to explain what's happening inline
* The arrow keys won't work in the REPL. Use backspace. If you get into a strange state with carriage returns and are unable to backspace, just hit return a few times, ignore any errors, and start again.
* The REPL will print ASCII values of integers. If you enter `65` you'll get back `%A`. Don't think about this too much yet. We'll cover this in detail on the next page.
* There is no input prompt in the REPL. And the result of the previous computation (after hitting `enter`) is just printed out on the next line. So if you're only reading and not following along live in the REPL, generally the first line of text you see in a code block is the user input, and the lines following it is the result of hitting the `enter` key.
* The single-character "symbolic operators" you'll see here are referred to as _runes_. They are core syntactic elements that perform various functions in Sire, like function application and data structure definition.

### Top-level binding - `=`

Binds a value globally - not scoped.

In the REPL:

```sire
; On the blank line in the REPL, type the following:
x=3
; And press enter.
; Now just type "x"...
x
; And press enter.
3 ; <-- the value returned from the REPL
```

### Function application - `|`, `()`

`add` is a function in Sire. It takes two arguments and adds them together. We apply `add` to `1` and `3` like so:

```sire
| add 1 3
; ^ function name (add)
;     ^ first argument (1)
;       ^ second argument (3)
4   ; return value
```

You can also apply functions with `()`:

```sire
(add 1 3)
4
```
This is actually a short form for `(| add 1 3)` - we're still doing function application with `|` under the hood, but the `|` rune is a special case that can be omitted in this context. All runes can be written several different ways (as you'll see next with the `=` rune).

Let's combine both of the above concepts to create our own named function. Note the way in this case you start the line with `=` (followed by a space) and the name of the function is the first value after the opening parenthesis.

```sire
; We're using the prefix notation for = here, which we saw in "infix" notation above at x=3
= (addTwo input)
| add 2 input
```

The REPL now has a function named `addTwo` bound to its top-level scope. The function takes a single value (called `input`) and all it does is apply (with `|`) the `add` function to two arguments, a hard-coded `2` and whatever input was provided.

```sire
(addTwo 4)
6  ; return value
```

### Scoped/Let binding - `@`

`@` binds a value to the present scope.\
We can see this by trivially modifying our `addTwo` function to bind an arbitrary name and use that for the addition:

```sire
= (addTwo originalInput)
@ renamedInput originalInput
| add 2 renamedInput

(addTwo 4)
6
```

On the second line, we bound a new name, `renamedInput` with the value of `originalInput`. Then `2` is added to it.

To prove to yourself that `renamedInput` is only bound within the function scope, try calling it at the top-level of the REPL:

```sire
renamedInput

++ %crash
++ {Failed to Parse Sire}
++   `   # block renamedInput
         # where REPL:214
         # problem renamedInput
         # reason
           {++ {Failed to Parse Sire}
++   `   # block renamedInput
         # where REPL:214
         # problem renamedInput
         # reason {Unresolved symbol: renamedInput}
```

## Natural numbers, byte-arrays and strings

### Nat - natural number

All values in Pallas are trees, whose leaves are natural numbers. Throughout the system, these are referred to as "nats".
The REPL will represent these values as ASCII for printing purposes, which can be confusing at first and should be noted up front. For instance, the character `a` is encoded as `97` in ASCII:

```sire
97
;; prints:
%a

| showNat %a
;; prints:
{97}
;; The showNat function represents a nat as a string.
;; Single curly braces wrapping a value means you're seeing a string.
;; Think of {curly braces} as you would "double quotes", as far as the
;; REPL is concerned. Same for the % leading the "a" a few lines above.
```

The full explanation is of how nats are rendered is [here](/deeper/nat-representations.md), but we suggest you continue along for now without getting too bogged down in the REPL representation of nats.

### Bar - byte-array, and Strings

A "bar" is an array of UTF-8 bytes. You create a bar like this:

```sire
b#{some stuff here}
```

You only need to use the curly braces when spaces are present in your byte array:

```
b#thisIsAFineBar
```

While it may be attractive to think about bars and strings interchangeably, they are not identical and when working between the two you'll often need to use conversion functions (like [natBar](/sire/standard-library.md#natBar) and [barNat](/sire/standard-library.md#barNat). There are more standard library functions for working with bars on a structural level (folding, splitting at indexes, filtering, etc.) while the string standard library functions are more geared to character-level functions like capitalization and checking if a character is an alphanumeric.  



Trust us that you can and should basically just use bars for everything string-like and you can move along to the section on data structures below, but if you're interested in seeing the deep dive, it's [here](/deeper/nat-representations.md).

## A Few Simple Data Structures

### Rows - `[]`

Rows/vectors are basically arrays (not Lists - see below). They are defined with `[ ]`:

```sire
arr=[10 64 42]

arr
[10 64 42]
```

We'll get into more of the standard library/convenience functions later, but we'll need a few now, too, in order to prove to ourselves some details of rows. `idx` is used to get the value at a given index in a row:

```sire
arr=[10 64 42]

(idx 0 arr)
10
; the zeroth item in the row

(idx 2 arr)
42

(idx 50 arr)
0
; you'll get zero back if you overshoot
```

`len` will give you the size of the row:

```sire
(len arr)
3
```

### Lists - `~[]`

Lists are zero-terminated, nested row 2-tuples. They are declared by prepending a `~` to what looks like row syntax, like this: `~[]` (in the REPL we have to wrap this in parentheses):

```sire
x=(~[10 64 42])
; Note, parentheses are added around the list because of the infix =
; You could also write this as:  = x ~[10 64 42]
; The choice between infix and prefix is most often one of visual clarity.

[10 [64 [42 0]]]

(idx 0 x)
10

(idx 1 x)
[64 [42 0]]
```

Incidentally, you can create a list from a row with the `listFromRow` function:

```
y=[2 3 4]
listy=(listFromRow y)

listy
[2 [3 [4 0]]]
```

## Comparison, Booleans and Maybes

`TRUE` is represented as the nat `1`, while `FALSE` is the nat `0`.

The `=?=` operation here is an assertion. It is often used for "unit testing". It takes two arguments and crashes if they are not equal, otherwise it evaluates without issue.

```sire
=?= 1 TRUE
=?= 0 FALSE
; These two expressions evaluate without issue.

=?= 1 FALSE
; This one crashes:
++ %crash
++ {Failed to Parse Sire}
++   `   # block
           =?= 1 | FALSE
         # where REPL:26
         # problem
           =?= 1 | FALSE
         # reason
           {++ {Failed to Parse Sire}
++   `   # block
           =?= 1 | FALSE
         # where REPL:26
         # problem
           =?=
             * 1
             * 0
         # reason {ASSERTION FAILURE}

```

### `eql` and friends

The `eql` function takes two parameters and returns `TRUE` if they're equal)

```sire
eql 1 1
1

eql 1 999
0
```

Similarly, we have `neq` for "not equal". Also:

- `lth` - true when first parameter is less than second
- `lte` - true when first parameter is less then or equal to second
- `gth` - true when first parameter is greater than second
- `gte` - true when first parameter is greater than or equal to second

### `if`

`if` is a function with three parameters: what is being tested for truthiness; what to return if true; what to return if false.

```sire
| if 1 {the if got a true} {the if got a false}
; returns:
{the if got a true}
```

The `if` function's first parameter was `1`, a true value, so the second parameter was returned.

### `not`

Negation.

```sire
| not FALSE
; returns:
1
```

### `and`, `or`

`and` and `or` are functions that do what you'd expect:

```sire
and 1 1
;; returns:
1

and 1 0
;; returns:
0

or 1 0
;; returns:
1

or 0 0
;; returns:
0
```

## Moving on

This was a brief overview of the nuts and bolts of Sire. Printing out bars in the REPL is fun and all, but our goal is to build a web app, not test the limit of how many "hello world" strings we can fit in our terminal scrollback.  

Next we'll take a look at a small sample of the standard library that we'll use while building our first Pallas Cog.
