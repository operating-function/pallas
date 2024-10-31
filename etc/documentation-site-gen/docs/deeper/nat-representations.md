# A deeper look at how nats are represented in bars, strings and hexidecimal

(This section assumes you've already gone through the [standard library tour](/sire/standard-library.md). If not, you'll see some functions here with which you're not familiar.)

## Another word on relationship between bars and strings

In the [intro](/sire/intro.md) we briefly covered bars and strings, but it's worth saying a bit more about them here. Because bars are _arrays of bytes_ (which we can interpret as UTF-8), and strings are stored as opaque nats, a bar representation and a string representation that look the same on the surface are _not_ equivalent PLAN values:

```sire
=?= b#{a string} {a string}
;; # {ASSERTION FAILURE}
```

The bar `a string` is not the same PLAN value as the string `a string`.

```sire
=?= (barNat b#{a string}) {a string}
;; Assertion succeeds
```

The `barNat` function will return the bar's string representation, which will indeed be the same nat as an identical string.

## Under the hood of strings and nats

```sire
showNat %a
{97}
```

We've [already](sire/intro.md) seen how the string `a` (entered above as `%a`) is "shown", stringified, as `{97}`. This is because we use ASCII to encode strings as nats, according to which the "string" `%a` is actually just the natural number `0x61`, or `97` in decimal form.

We can confirm this using hexadecimal notation:
```sire
0x61
%a
```

So what's happening is that when we want to print the string `%a`, that's actually asking to print the hexadecimal number `0x61`, which is the same as the decimal number `97`. So we get back the string `{97}`.


So what does the string `aa` look like? `97` twice? maybe the sum of `97` + `97`...?

```sire
showNat %aa
{24929}
```

Okay, that's surprising! To understand what's going on here, remember that ASCII is typically written in hexadecimal notation since that allows bits to line up neatly with the individual digits. So `%aa` should be the same as `0x6161`, which it seems to be:

```sire
0x6161
%aa
```

And the nat `24929` turns out to be the same value:

```sire
24929
%aa
```

There are two lessons to learn here: one is that at the end of the day, everything is stored as a natural number in memory, but these values can be presented different ways depending on the task at hand. The other is that the REPL can be misleading if you don't track _exactly_ what it's doing.
