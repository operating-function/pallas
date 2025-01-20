# Pads

Pads are bit-strings encoded as non-zero natural numbers. Pads are not data-jetted, since the direct nat works perfectly. Pads are encoded least-significant-bit-first, with the high bit used to indicate the end of the bit-array.

```
p#{}          == 0b1 ; 1
p#{00}        == 0b100 ; 4
p#{01}        == 0b110 ; 6
p#{111000}    == 0b1000111 ; %G
```

All of the pad operations coerce their inputs into pads, and always return pads.

::::info
The REPL will print pads as their natural number representation by default. Use `padShowLit` to coerce the output into the bit-string format used here.
::::

### emptyPad

Represents an empty pad.

```sire
emptyPad    == 1
```

### toPad

Coerces a value into a non-zero natural number (pad).

```sire
toPad 0     == 1
toPad 1     == 1
toPad 3     == 3
```

### padNat

Converts a pad to a natural number, dropping all trailing zeros.

```sire
padNat p#0      == 0
padNat p#1      == 1
padNat p#100    == 1
```

### natPad

Converts a natural number into a pad with a specific minimum bit-width.

```sire
natPad 1 3    == p#100
natPad 2 4    == p#0100
natPad 3 2    == p#11
```

### padLen

Returns the length of a pad (number of bits).

```sire
padLen p#0       == 1
padLen p#1       == 1
padLen p#1010    == 4
```

### padWeld

Concatenates two pads.

```sire
padWeld p#10 p#11      == p#1011
padWeld p#0 p#1        == p#01
padWeld p#111 p#000    == p#111000
```

### padCat

Concatenates a row of pads.

```sire
padCat [p#1 p#0 p#11]    == p#1011
padCat [p#10 p#01]       == p#1001
padCat []                == p#{}
```

### padFlat

Flattens and concatenates a nested structure of pads.

```sire
padFlat [[p#1 p#0] [p#1 p#1]]    == p#1011
padFlat [p#10 p#11]              == p#1011
padFlat [[[]] []]                == p#{}
```

### padSplitAt

Splits a pad at a given index.

```sire
padSplitAt 2 p#1111    == [p#11 p#11]
padSplitAt 1 p#1010    == [p#1 p#010]
padSplitAt 3 p#1001    == [p#100 p#1]
```

### padIdx

Returns the nth bit from a pad.

```sire
padIdx 0 p#1010    == 1
padIdx 1 p#1010    == 0
padIdx 3 p#1010    == 0
```

### padGet

Alias for padIdx, but with arguments flipped.

```sire
padGet p#1010 0    == 1
padGet p#1010 1    == 0
padGet p#1010 3    == 0
```

### padSet

Sets the nth bit in a pad using a Bit.

```sire
padSet p#0000 2 1    == p#0010
padSet p#1111 1 0    == p#1011
padSet p#1010 3 1    == p#1011
```

### padMapWithKey

Maps a function over the bits in a pad, providing both the index and the bit value.

```sire
padMapWithKey (k b & if even-k b not-b) p#0000    == p#0101
padMapWithKey (k b & if even-k b not-b) p#1111    == p#1010
padMapWithKey (k b & add k b) p#1010              == p#1111
```

### padMap

Maps a function over the bits in a pad, coercing outputs to bits.

```sire
padMap not p#1010          == p#0101
padMap (add 1) p#1010      == p#1111
padMap (const 0) p#1111    == p#0000
```

### padComplement

Complements all bits in a pad.

```sire
padComplement p#1010    == p#0101
padComplement p#1111    == p#0000
padComplement p#0000    == p#1111
```

### showPadStr

Converts a pad to its string representation.

```sire
showPadStr p#1010    == {0101}
showPadStr p#1000    == {0001}
showPadStr p#{}      == 0
```

### showPadLit

Converts a pad to its Rex literal representation.

```sire
showPadLit p#1010    == p#0101
showPadLit p#1000    == p#0001
showPadLit p#{}      == p#{}
```

