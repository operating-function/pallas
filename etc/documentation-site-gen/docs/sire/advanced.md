---
description: 'Document Type: Reference'
---

# Recognize and move on

The topics here are explained so that you can recognize them if you come across them, but don't worry about fully understanding and using everything you see here. For the purposes of getting familiar with the system and building some toy cogs, you can get away with copying existing instances of these patterns and changing them subtly to your needs.  
Eventually you should understand everything you're doing, but let's keep you moving forward first.

## Alternative function application syntax

`-` can be used for function application. It's less commonly seen, but you should be aware of it so you understand it when you see it somewhere. It is most often used with single argument function application, like `f-x`.

```sire
inc-1
2
```

## Types

Sire _sort of_ has types at the moment. This system will significantly change for the better in the future, but as a beginner we suggest you just ignore types for now. That said, you should at least be familiar with the syntax.

```sire
> Bar > Bar > Nat > Bit
= (barIsPrefixOf needle haystack off)
| eql needle
| barSlice off (barLen needle) haystack
```

This function takes a sought term "needle" and a target "haystack" and returns a boolean of whether or not this "needle" is the prefix of the "haystack" (at some offset).

This line is the type signature:

```sire
> Bar > Bar > Nat > Bit
```

First parameter is a bar ("needle"), second parameter is a bar ("haystack"), third parameter is a nat (offset) and the function returns a bit (boolean).

## Lambdas

- `&`  Anonymous lambda
- `?`  Named lambda
- `??` Named and pinned lambda (Pinning has to do with helping with memory layout in persistence. Don't worry about it too much for now, but you'll come across it in source files)

## Col macro

"Col" as in "colon"

```sire
: (a b c) < foo x y
; the rest of the computation below
```
- `foo x y` is the function call. It accepts another function as an argument - the callback below:
- `(a b c)` `a b c` are the arguments to the callback
- The main function and callback can have any arguments - these were chosen arbitrarily for this example

We definitely need an example to better understand this. We'll use `gen` for the example (`gen` was covered in the [standard library](/sire/standard-library.md))

```sire
; we saw gen used like this:
gen 10 | mul 2
[0 2 4 6 8 10 12 14 16 18]

; Here it is with col:

: index < gen 5
| mul index 2
[0 2 4 6 8]

; and an example with maybeCase

maybeNine=(SOME 9)
: actuallyNine < maybeCase maybeNine {wasn't nine}
| inc actuallyNine

; returns:
10

maybeNine=NONE
: actuallyNine < maybeCase maybeNine {wasn't nine}
| inc actuallyNine

; returns:
{wasn't nine}
```

This might look trivial (after all, you could have written `maybeCase (SOME 9) wasn't nine} inc` and arrived at `10` without using the col macro) so to drive the point home a bit more: Notice that in the next example, the remainder of the continued computation is acting on `actuallyNine` without having to use a let binding or order the function application pipeline strangely.

```sire
maybeNine=(SOME 9)
: actuallyNine < maybeCase maybeNine {no}
| add 1 | div actuallyNine 3

; result:
4
```

## `PIN`

Another one to note and ignore. All you need to know now is that `PIN`s help with memory-layout optimization during persistence. You can look at it like a `let` binding for now without being too far off the mark.

---

Next up, running a simple Cog:
