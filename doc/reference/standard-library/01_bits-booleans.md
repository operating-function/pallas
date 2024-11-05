---
title: Bits (Booleans)
---

# Bits (Booleans)

::::warning
Type signatures are provisional and may contain errors.
::::

::::warning
Sire has an incomplete type system. No type checks are enforced at this point. Type signatures are provided for the intended behavior.
::::

## Constants

### TRUE

Represents the boolean value true.

```sire
TRUE    == 1
```

### FALSE

Represents the boolean value false.

```sire
FALSE    == 0
```

## Conditionals

### if

```
(if x t e)
> x : Bool
> t : a
> e : a
> a
```

Conditional operation. If the condition is true (non-zero), returns the second argument, otherwise returns the third argument.

```sire
if 1 {yes} {no}    == %yes
if 0 {yes} {no}    == %no
if TRUE 1 2        == 1
if FALSE 1 2       == 2
```

### ifNot

```
(ifNot x t e)
> x : Bool
> t : a
> e : a
> a
```

Inverted conditional. If the condition is false (zero), returns the second argument, otherwise returns the third argument.

```sire
ifNot 1 {yes} {no}    == %no
ifNot 0 {yes} {no}    == %yes
ifNot TRUE 1 2        == 2
ifNot FALSE 1 2       == 1
```

### ifz

```
(ifz x t e)
> x : a
> t : a
> e : a
> a
```

Conditional based on zero. If the first argument is zero, returns the second argument, otherwise returns the third argument.

```sire
ifz 0 b#zero b#nonzero     == b#zero
ifz 1 b#zero b#nonzero     == b#nonzero
ifz 42 b#zero b#nonzero    == b#nonzero
```

### ifNonZero

```
(ifNonZero x t e)
> x : a
> t : a
> e : a
> a
```

Conditional based on non-zero. If the first argument is non-zero, returns the second argument, otherwise returns the third argument.

```sire
ifNonZero 0 b#zero b#nonzero     == b#nonzero
ifNonZero 1 b#zero b#nonzero     == b#zero
ifNonZero 42 b#zero b#nonzero    == b#zero
```

### else

Identity function, used to improve readability in conditional expressions.

```sire
else 10    == 10
```

## Bit Operations

### bit

```
(bit x)
> x : Bool
> Bool
```

Converts a value to a bit (0 or 1).

```sire
bit 0        == 0
bit 1        == 1
bit 42       == 1
bit FALSE    == 0
bit NIL      == 0
```

### not

```
(not x)
> x : Bool
> Bool
```

Logical NOT operation.

```sire
not 0        == 1
not 1        == 0
not 42       == 0
not FALSE    == 1
not TRUE     == 0
```

### and

```
(and x y)
> x : Bool
> y : Bool
> Bool
```

Logical AND operation.

```sire
and 0 0           == 0
and 0 1           == 0
and 1 0           == 0
and 1 1           == 1
and TRUE FALSE    == 0
and TRUE TRUE     == 1
```

### or

```
(or x y)
> x : Bool
> y : Bool
> Bool
```

Logical OR operation.

```sire
or 0 0            == 0
or 0 1            == 1
or 1 0            == 1
or 1 1            == 1
or TRUE FALSE     == 1
or FALSE FALSE    == 0
```

### xor

```
(xor x y)
> x : Bool
> y : Bool
> Bool
```

Logical XOR (exclusive OR) operation.

```sire
xor 0 0           == 0
xor 0 1           == 1
xor 1 0           == 1
xor 1 1           == 0
xor TRUE FALSE    == 1
xor TRUE TRUE     == 0
```

### nand

```
(nand x y)
> x : Bool
> y : Bool
> Bool
```

Logical NAND (NOT AND) operation.

```sire
nand 0 0           == 1
nand 0 1           == 1
nand 1 0           == 1
nand 1 1           == 0
nand TRUE FALSE    == 1
nand TRUE TRUE     == 0
```

### nor

```
(nor x y)
> x : Bool
> y : Bool
> Bool
```

Logical NOR (NOT OR) operation.

```sire
nor 0 0            == 1
nor 0 1            == 0
nor 1 0            == 0
nor 1 1            == 0
nor TRUE FALSE     == 0
nor FALSE FALSE    == 1
```

### xnor

```
(xnor x y)
> x : Bool
> y : Bool
> Bool
```

Logical XNOR (NOT XOR) operation.

```sire
xnor 0 0           == 1
xnor 0 1           == 0
xnor 1 0           == 0
xnor 1 1           == 1
xnor TRUE FALSE    == 0
xnor TRUE TRUE     == 1
```
