---
description: 'Document Type: Reference'
---

# Some Standard Library

Our goal at this point is to get you familiar with enough of the Sire standard library that you can write a basic cog. As such, this is not an exhaustive reference but an overview of common functions.

Feel free to skim this section for now and come back to it as needed while you work through the rest of the docs.

### `trk`

This is a printf, a `console.log`.

```Sire
(trk %something)

:: prints:
(_Trace %hello)
```

```Sire
= x %hello_world
| trk [%someMessage x]

;; prints:
trk=[%someMessage {hello_world}]
```

### `=?=`

Assertion. Crashes when the assertion is false. Often used for basic unit tests.

```sire
;; I say 1 is equal to 2!
=?= 1 2

;; Sire says it isn't:
++ %crash
++ {Failed to Parse Sire}
++   `   # block
           =?= 1 2
         # where REPL:35
         # problem
           =?= 1 2
         # reason
           {++ {Failed to Parse Sire}
++   `   # block
           =?= 1 2
         # where REPL:35
         # problem
           =?=
             * 1
             * 2
         # reason {ASSERTION FAILURE}
}


;; I say {hello} is equal to {hello}
=?= {hello} {hello}

;; The REPL agrees by not crashing and printing the assertion again
=?= {hello} {hello}
```

## Nats

### Basic arithmetic

We won't cover all the basic arithmetic functions because they're quite straightforward. Here is a list of the common operations: `inc` `dec` `add` `sub` `mod` `mul` `div` `pow`.

### `showNat`

Prints a nat as a string. Mostly used in the REPL.&#x20;

```sire
showNat 100
{100}

showNat "a"
{97}

showNat "aa"
{24929}
```

For more information on Nat representations, [read this](../deeper/nat-representations.md).

### `natBar`

Given a nat, returns its bar representation.

```sire
natBar 97
b#a
```

## Bars

### `barNat`

Given a bar, returns it as a nat.

```sire
barNat b#a
%a
;; remember, this is the nat 97 but printed in stringified (ASCII) form

barNat x#1
1
barNat x#9
9
barNat x#a
10
;; x# is representing a byte-array as hexidecimal, in which the
;; decimal number 10 is represented as hex 'a'.
```

### `barLen`

Given a bar, returns the length of the byte array.

```sire
barLen b#{}
0

barLen b#{a}
1

barLen b#{aaaaa}
5
```

### `barCat`

Given a row of bars, return their concatenation.

```sire
= b1 b#{hello}
= b2 b#{world}

| barCat [b1 b#{ } b2]

;; returns:
 b#{hello world}
```

### `barCatList`

Similar to `barCat`, but operates on a list, rather than a row (note the `~` in `~[]` below).

```sire
= b1 b#{hello}
= b2 b#{world}

| barCatList ~[b1 b#{ } b2]

;; returns:
 b#{hello world}
```

## SOME, "Maybe"

Useful when you're not sure if you're going to have a value.\
"Maybes" are represented as a pair of `(0 <your-thing>)`. This is called `SOME` in sire. For convenience, we also have `NONE`, which is really just the nat `0`.

```sire
; bind "maybe 9..." to "som"
= som (SOME 9)

; bind the NONE representation to "non" for parity with above
= non NONE

; look at it:
som
(0 9)

non
0
```

Now we can do some operations on these:

### `isNone`, `isSome`

```sire
isSome som
1 ; true

isSome non
0 ; false

isNone non
1 ; true

isNone som
0 ; false
```

### `fromSome`, `unpackSome`

`fromSome` returns the value of the `SOME`, otherwise returns the first parameter.

```sire
fromSome {got nothin} som
9

fromSome {got nothin} non
{got nothin}
```

`unpackSome` also returns the value of the `SOME`, but doesn't have a guard - it just crashes if given a `NONE`

```sire
unpackSome som
9

unpackSome non

++ %crash
++ {Failed to Parse Sire}
++   `   # block
             | unpackSome non
         # where REPL:220
         # problem
             | unpackSome non
         # reason {[%die {Unexpected NONE}]
}
```

### `maybeCase`

`maybeCase` takes a maybe, a guard for `NONE` and a function to call on the value in the case of a `SOME`.

```sire
; if we have a value, we'll call the increment function on it
; (remember our "som" maybe had the value 9)

maybeCase som {got nothing} inc
10

maybeCase non {got nothing} inc
{got nothing}
```

### `fmapMaybe`

`fmapMaybe` takes a maybe and a function to call on it, but preserves the maybe form for the return value.

```sire
; continuing to use the increment example
fmapMaybe som inc
(0 10) ; a SOME

fmapMaybe non inc
0      ; a NONE
```

## Tabs

A tab is a map from noun to noun. Like a dict in Haskell or Python.

### Create a tab: `#[]` / `tabFromPairs`

To create a tab, you can use the `tabFromPairs` function or the `#[]` syntax.\
`tabFromPairs` accepts two rows of nats for the keys and values, while `#[]` works more like a row of bindings.

```sire
= t | tabFromPairs [{one} {two} {three}] [1 2 3]
;; returns:
t=[one=1 two=2 three=3]

;; using the #[] syntax:
= t #[{one}=1 {two}=2 {three}=3]
;; returns:
t=[one=1 two=2 three=3]
```

In the above example, `one`, `two` and `three` are string keys; `1` `2` and `3` are nat values.

:exclamation:Caution: While any nat can be used for keys and values, you might run into syntax issues when using the `#[]` structure:

```sire
;; this is a valid tab:
= t | _MkTab [b#{I'm a bar} {I'm a string}] [b#{bar value} {string value}]

;; but the same keys/values will choke the #[] version:
= t | #[b#{I'm a bar}={I'm a string}] [b#{bar value}={string value}]
++ %crash
++ {Failed to Parse Sire}
```

### `tabFromPairs`

Another way to construct a tab from a row of pairs:

```sire
= t | tabFromPairs [[{one} 1] [{two} 2] [{three} 3]]
[one=1 two=2 three=3]
```

### `tabGet`

Get the value at a key in a tab, return `0` if the key doesn't exist.

```sire
tabGet t {two}
2

tabGet t {i dont exist}
0
```

### `tabLookup`

Similar to above, but returns maybes when hunting for keys in tabs

```sire
t
[one=1 two=2 three=3]

tabLookup {one} t
(0 1)

tabLookup t {noKey}
0

; we can apply fromSome to this:

fromSome {not found} | tabLookup {one} t
1

fromSome {not found} | tabLookup {noKey} t
{not found}
```

## A few Row functions

### `gen`

Return a row of n length, generated by function passed. `gen` passes the item's indexes in the row to the function.

```sire
; give me a row with 10 members, calling the "id" function on each index
; ("id" just returns the value it was passed)

gen 10 id
[0 1 2 3 4 5 6 7 8 9]

gen 10 inc
[1 2 3 4 5 6 7 8 9 10]

gen 10 | mul 2
[0 2 4 6 8 10 12 14 16 18]
```

### `map`

Apply a function to all values in a row

```sire
map inc [0 1 2 3]
[1 2 3 4]
```

### `weld`

Concatenate two rows

```sire
weld [1 2] [3 4]
[1 2 3 4]
```

## Finding additional standard library functions

The entire "standard library" is defined in the consecutively-numbered `sire/sire_<n>_<name>.sire` files. If you're trying to complete some task and the functions described above don't help, there's a decent chance there's already a function defined in the standard library that will help.

Do a text-search on the `*.sire` files for a term that seems relevant (`slice`, `sum`, etc.) and look around nearby. Check out the list of imports at the top of the file to see what _this_ file depends on. Consult the list of sire files below and skim the files that might seem useful. When first encountering a file we suggest you skim the list of exports at the bottom (after reading any initial comment at the very top) to get a sense of the functions this file exports. Often, the `=?=` test cases can be very helpful.

### Rough explanation of the standard library files

Below is a summary of each of the standard library files. Particularly helpful ones for a beginner are annotated with a 👍. Files that require a more advanced understanding and can be skipped for now are annotated with a ❗. Most of these files primarily define lower-level dependencies that other higher-order (and easier to understand) subsequent functions rely on.

* `sire_01_fan.sire` - Defines named wrappers around PLAN operations
* `sire_02_bit.sire` - Booleans
* `sire_03_nat.sire` - Natural numbers and operating on them
* `sire_04_cmp.sire` - Comparison, ordering and equality
* `sire_05_row.sire` - Rows and basic operations on them
* ❗ `sire_06_rex.sire` - Representation for rex trees - mostly needed for macros.
* 👍 `sire_07_dat.sire` - Data structures; rows, lists, maybe, either, etc.
* 👍 `sire_10_str.sire` - ASCII characters and strings
* `sire_11_set.sire` - Sets
* 👍 `sire_12_tab.sire` - Tabs
* ❗ `sire_13_exp.sire` - More rex and macro utilities
* ❗ `sire_14_hax.sire` - Explains how the `#` rune is used for macros
* ❗ `sire_15_pad.sire` - Bit-strings encoded as nats
* 👍 `sire_16_bar.sire` - Byte-arrays and operations
* `sire_17_sug.sire` - Syntactic sugar and convenience macros
* ❗ `sire_18_pat.sire` - Pattern matching
* ❗ `sire_19_bst.sire` - Binary search trees
* ❗ `sire_20_prp.sire` - Sire properties
* `sire_21_switch.sire` - Atomic switch
* ❗ `sire_22_seed.sire` - Seed; serialization framework
* ❗ `sire_23_repl.sire` - REPL utilities
* ❗ `sire_24_rex.sire` - Rex
* `sire_25_datatype.sire` - Datacase/Record
* ⁉️ `sire_26_compile.sire` - Backend of the sire compiler
* ⁉️ `sire_27_sire.sire` - Sire-in-sire; can be used to bootstrap itself

Taking a look at this list above, you can also get a sense of how the Sire source files start at the basics of wrapping PLAN and incrementally build on each other until the full system is realized.\
By starting with PLAN and going through all the files above, after a (relatively) small investment of time, you could understand the **entirety** of this computational model. Pretty cool.

Next we'll introduce some more advanced topics that you'll come across:
