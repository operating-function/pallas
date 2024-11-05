# Bars (byte arrays)

### {#b}
Macro for creating ASCII bar literals.

```sire
# b {hello}    == b#hello
# b {}         == b#{}
# b {123}      == b#123
```

### {#x}
Macro for creating hexadecimal bar literals.

```sire
# x deadbeef    == x#deadbeef
# x 00          == x#00
# x {}          == b#{}
```

### padBar

```
(padBar p)
> p : Pad a
> Bar a
```

Converts a pad to a bar, adding zeroes to make it into whole bytes if necessary.

```sire
padBar p#11111111     == x#ff
padBar p#111111111    == x#ff01
padBar p#1            == x#01
```

### isBar

Checks if a value is a bar (byte array).

```sire
isBar b#hello       == 1
isBar x#deadbeef    == 1
isBar 42            == 0
```

### emptyBar

Returns an empty bar.

```sire
emptyBar               == b#{}
barLen emptyBar        == 0
barIsEmpty emptyBar    == 1
```

### barIsEmpty

Checks if a bar is empty.

```sire
barIsEmpty b#{}    == 1
barIsEmpty b#a     == 0
barIsEmpty x#00    == 0
```

### byteWidth

Returns the number of bytes needed to represent a natural number.

```sire
byteWidth 0      == 0
byteWidth 255    == 1
byteWidth 256    == 2
```

### barTrail

Returns the number of trailing zero bytes in a bar's internal representation.

```sire
barTrail 0x1ff        == 0
barTrail 0x100ff      == 1
barTrail 0x10000ff    == 2
```

### barEnc

Encodes a natural number into a bar representation.

```sire
barEnc 0 0      == 1
barEnc 0 1      == 257
barEnc 0 255    == 511
```

### barDec

Decodes a bar representation back into a natural number.

```sire
barDec 1      == 0
barDec 257    == 1
barDec 511    == 255
```

### mkBar

Creates a bar from a natural number and a trailing zero count.

```sire
mkBar 0 0      == b#{}
mkBar 0 1      == x#01
mkBar 1 255    == x#ff00
```

### natBar

Creates a bar from a natural number.

```sire
natBar 0      == b#{}
natBar 1      == x#01
natBar 255    == x#ff
```

### barNat

Converts a bar to its natural number representation.

```sire
barNat b#{}    == 0
barNat x#01    == 1
barNat x#ff    == 255
```

### barLen

Returns the length of a bar in bytes.

```sire
barLen b#{}          == 0
barLen b#hello       == 5
barLen x#deadbeef    == 4
```

### barIdx

Returns the byte at a given index in a bar.

```sire
barIdx 0 b#hello    == %h
barIdx 1 b#hello    == %e
barIdx 4 b#hello    == %o
```

### natToSizedBar

Creates a bar of a specific size from a natural number.

```sire
natToSizedBar 4 0x11223344    == x#44332211
natToSizedBar 2 0x1122        == x#2211
natToSizedBar 4 0x11          == x#11000000
```

### barSing

Creates a bar containing a single byte.

```sire
barSing %a     == b#a
barSing 0      == x#00
barSing 255    == x#ff
```

### barDuo

Creates a bar containing two bytes.

```sire
barDuo %a %b      == b#ab
barDuo 0 1        == x#0001
barDuo 255 254    == x#fffe
```

### zEnd

Returns the number of trailing zero bytes in a row.

```sire
zEnd [1 2 3 0 0]    == 2
zEnd [0 0 0]        == 3
zEnd [1 2 3]        == 0
```

### stripZEnd

Removes trailing zero bytes from a row.

```sire
stripZEnd [1 2 3 0 0]    == [1 2 3]
stripZEnd [0 0 0]        == []
stripZEnd [1 2 3]        == [1 2 3]
```

### w8

Converts a number to an 8-bit value (0-255).

```sire
w8 255    == 255
w8 256    == 0
w8 257    == 1
```

### bytesBar

Creates a bar from a row of bytes.

```sire
bytesBar [104 101 108 108 111]    == b#hello
bytesBar [0 255 1]                == x#00ff01
bytesBar []                       == b#{}
```

### barGen

Generates a bar by applying a function to each index.

```sire
barGen 3 (mul 2)    == x#000204
barGen 4 id         == x#00010203
barGen 0 id         == b#{}
```

### barRep

Creates a bar by repeating a byte value.

```sire
barRep 3 %a    == b#aaa
barRep 2 0     == x#0000
barRep 0 %x    == b#{}
```

### barGet

Gets the byte at a specific index in a bar.

```sire
barGet b#hello 0       == %h
barGet b#hello 4       == %o
barGet x#deadbeef 2    == 190 ; [222 173 190 239]
```

### barBytes

Converts a bar to a row of bytes.

```sire
barBytes b#hello       == [%h %e %l %l %o]
barBytes x#deadbeef    == [222 173 190 239]
barBytes b#{}          == []
```

## barFoldl

Left-associative fold over a bar.

```sire
barFoldl add 0 b#abc       == 294
barFoldl add 1 b#{}        == 1
barFoldl mul 1 x#010203    == 6
```

## barFoldr

Right-associative fold over a bar.

```sire
barFoldr add 0 b#abc                    == 294
barFoldr (_ acc & inc acc) 0 b#hello    == 5
barFoldr mul 1 x#010203                 == 6
```

### barAny

Checks if any byte in the bar satisfies a predicate.

```sire
barAny (eql %a) b#hello        == 0
barAny (eql %h) b#hello        == 1
barAny (gte 200) x#deadbeef    == 1
```

### barAll

Checks if all bytes in the bar satisfy a predicate.

```sire
barAll (gte %a) b#hello        == 0
barAll (eql %a) b#aaaaa        == 1
barAll (lte 200) x#deadbeef    == 0
```

### barWeld

Concatenates two bars.

```sire
barWeld b#hello b#world    == b#helloworld
barWeld x#dead x#beef      == x#deadbeef
barWeld b#{} b#abc         == b#abc
```

### barCat

Concatenates a row of bars.

```sire
barCat [b#hello b#world b#{!}]    == b#{helloworld!}
barCat [x#de x#ad x#be x#ef]      == x#deadbeef
barCat [b#{} b#a b#{} b#b]        == b#ab
```

### barCatList

Concatenates a list of bars.

```sire
barCatList (CONS b#hello (CONS b#world NIL))    == b#helloworld
barCatList (CONS x#dead (CONS x#beef NIL))      == x#deadbeef
barCatList NIL                                  == b#{}
```

### barCatMap

Maps a function over a bar and concatenates the results.

```sire
barCatMap (x & barRep 2 x) b#abc          == b#aabbcc
barCatMap (x & barFromRow [x x]) b#123    == b#112233
barCatMap (const b#{-}) b#abc             == b#{---}
```

### barIntercalate

Intersperses a separator bar between the elements of a row of bars.

```sire
barIntercalate b#_ [b#a b#b b#c]         == b#a_b_c
barIntercalate x#00 [x#11 x#22 x#33]     == x#110022003300
barIntercalate b#{} [b#hello b#world]    == b#helloworld
```

### barIntercalateList

Intersperses a separator bar between the elements of a list of bars.

```sire
barIntercalateList b#_ ~[b#a b#b b#c]                              == b#a_b_c
barIntercalateList x#00 (CONS x#11 (CONS x#22 (CONS x#33 NIL)))    == x#110022003300
barIntercalateList b#{} (CONS b#hello (CONS b#world NIL))          == b#helloworld
```

### barFromHex

Creates a bar from a hexadecimal string.

```sire
barFromHex {deadbeef}      == x#deadbeef
barFromHex {68656C6C6F}    == b#hello
barFromHex {}              == b#{}
```

### barPad

Returns the internal pad representation of a bar.

```sire
barPad b#a           == p#10000110
barPad x#deadbeef    == p#01111011101101010111110111110111
barPad b#{}          == p#{}
```

### barTake

Takes the first n bytes from a bar.

```sire
barTake 3 b#hello    == b#hel
barTake 5 b#hi       == b#hi
barTake 0 b#world    == b#{}
```

### barDrop

Drops the first n bytes from a bar.

```sire
barDrop 3 b#hello    == b#lo
barDrop 5 b#hi       == b#{}
barDrop 0 b#world    == b#world
```

### barSlice

Takes a slice of a bar from a starting index with a given length.

```sire
barSlice 1 3 b#hello       == b#ell
barSlice 0 2 x#deadbeef    == x#dead
barSlice 4 1 b#hello       == b#o
```

### barSliceToNat

Converts a slice of a bar to a natural number.

```sire
barSliceToNat 0 4 x#deadbeef    == 0xdeadbeef
barSliceToNat 1 2 x#deadbeef    == 0xadbe
barSliceToNat 3 1 b#hello       == 0x6f
```

### barTreeToList

Converts a tree-like structure of bars to a flat list of bars.

```sire
barTreeToList b#hello                      == [b#hello 0]
barTreeToList [b#a b#b b#c]                == [b#a [b#b [b#c 0]]]
barTreeToList (0 [[b#foo] b#bar b#baz])    == [b#foo [b#bar [b#baz 0]]]
```

### barFlat

Flattens a tree-like structure of bars into a single bar.

```sire
barFlat b#hello                      == b#hello
barFlat [b#a b#b b#c]                == b#abc
barFlat (0 [[b#foo] b#bar b#baz])    == b#foobarbaz
```

### barHas

Checks if a bar contains a specific byte.

```sire
barHas %h b#hello         == 1
barHas %x b#hello         == 0
barHas 0xde x#deadbeef    == 1
```

### barToRow

Converts a bar to a row of bytes (alias for barBytes).

```sire
barToRow b#hello       == [%h %e %l %l %o] ; [104 101 108 108 111]
barToRow x#deadbeef    == [222 173 190 239]
barToRow b#{}          == []
```

### rowFromBar

Converts a bar to a row of bytes (alias for barBytes).

```sire
rowFromBar b#hello       == [%h %e %l %l %o]
rowFromBar x#deadbeef    == [222 173 190 239]
rowFromBar b#{}          == []
```

### barToList

Converts a bar to a list of bytes.

```sire
barToList b#abc      == [%a [%b [%c 0]]]
barToList x#1234     == [18 [52 0]] ; [0x12 [0x34 0]]
barToList b#{}       == 0
```

### barFromRow

Creates a bar from a row of bytes.

```sire
barFromRow [104 101 108 108 111]    == b#hello
barFromRow [0xde 0xad 0xbe 0xef]    == x#deadbeef
barFromRow []                       == b#{}
```

### barFromList

Creates a bar from a list of bytes.

```sire
barFromList [%a [%b [%c 0]]]    == b#abc
barFromList [0x12 [0x34 0]]     == x#1234
barFromList 0                   == b#{}
```

### barFromListRev

Creates a bar from a reversed list of bytes.

```sire
barFromListRev [%c [%b [%a 0]]]    == b#abc
barFromListRev [0x34 [0x12 0]]     == x#1234
barFromListRev 0                   == b#{}
```

### rowToBar

Creates a bar from a row of bytes (alias for barFromRow).

```sire
rowToBar [104 101 108 108 111]    == b#hello
rowToBar [0xde 0xad 0xbe 0xef]    == x#deadbeef
rowToBar []                       == b#{}
```

### barMap

Applies a function to each byte in a bar.

```sire
barMap (add 1) b#abc        == b#bcd
barMap (mul 2) x#0102       == x#0204
barMap (const 0) b#hello    == x#0000000000
```

### barFilter

Keeps only the bytes in a bar that satisfy a predicate.

```sire
barFilter (neq %l) b#hello         == b#heo
barFilter (gte 0xc0) x#deadbeef    == x#adbe
barFilter (const 1) b#abc          == b#abc
```

### barFindIndex

Finds the index of the first byte satisfying a predicate.

```sire
barFindIndex (eql %l) b#hello         == (0 2) ; SOME 2
barFindIndex (gte 0xc0) x#deadbeef    == (0 1) ; SOME 1
barFindIndex (const 0) b#abc          == 0 ; NONE
```

### barFindIndexOff

Finds the index of the first byte satisfying a predicate, starting from an offset.

```sire
barFindIndexOff (eql %l) 1 b#hello         == 2
barFindIndexOff (gte 0xc0) 1 x#deadbeef    == 1
barFindIndexOff (const 0) 0 b#abc          == 3
```

### barElemIndex

Finds the index of the first occurrence of a byte.

```sire
barElemIndex %l b#hello         == (0 2) ; SOME 2
barElemIndex 0xde x#deadbeef    == (0 0) ; SOME 0
barElemIndex %x b#abc           == 0 ; NONE
```

### barElemIndexOff

Finds the index of the first occurrence of a byte, starting from an offset.

```sire
barElemIndexOff %l 1 b#hello         == 2
barElemIndexOff 0xde 1 x#deadbeef    == 4
barElemIndexOff %x 0 b#abc           == 3
```

### barElemIndexEnd

Finds the index of the last occurrence of a byte.

```sire
barElemIndexEnd %l b#hello         == (0 3) ; SOME 3
barElemIndexEnd 0xef x#deadbeef    == (0 3) ; SOME 3
barElemIndexEnd %x b#abc           == (0 0) ; NONE
```

### barSplitAt

Splits a bar at a given index.

```sire
barSplitAt 2 b#hello       == [b#he b#llo]
barSplitAt 4 x#deadbeef    == [x#deadbeef b#{}]
barSplitAt 1 b#abc         == [b#a b#bc]
```

### barSplitOne

Splits a bar at the first occurrence of a byte.

```sire
barSplitOne %l b#hello         == [b#he b#lo]
barSplitOne 0xad x#deadbeef    == [x#de x#beef]
barSplitOne %x b#abc           == [b#abc b#{}]
```

### barSplitOneWith

Splits a bar at the first occurrence of a byte satisfying a predicate.

```sire
barSplitOneWith (eql %l) b#hello         == [b#he b#lo]
barSplitOneWith (gte 0xc0) x#deadbeef    == [x#de x#beef]
barSplitOneWith (const 0) b#abc          == [b#abc b#{}]
```

### barSplitWith

Splits a bar at all occurrences of bytes satisfying a predicate.

```sire
barSplitWith (eql %l) b#hello         == [b#he [b#{} [b#o 0]]]
barSplitWith (gte 0xc0) x#deadbeef    == [x#de [b#{} [x#ef 0]]]
barSplitWith (const 0) b#abc          == [b#abc 0]
```

### barSplit

Splits a bar at all occurrences of a specific byte.

```sire
barSplit %l b#hello         == [b#he [b#{} [b#o 0]]]
barSplit 0xad x#deadbeef    == [x#de [x#beef 0]]
barSplit %x b#abc           == [b#abc 0]
```

### barCountHeadMatching

Counts the number of leading bytes until a predicate fails.

```sire
barCountHeadMatching (eql %h) b#hello         == (0 1) ; SOME 1
barCountHeadMatching (neq 0xbe) x#deadbeef    == (0 2) ; SOME 2
barCountHeadMatching (neq %d) b#abc           == 0 ; NONE
```


### barDropWhile

Drops leading bytes from a bar while they satisfy a predicate.

```sire
barDropWhile (eql %h) b#hello         == b#ello
barDropWhile (lte 0xbe) x#deadbeef    == x#adbeef
barDropWhile (const 0) b#abc          == b#abc
```

### barTakeWhile

Takes leading bytes from a bar while they satisfy a predicate.

```sire
barTakeWhile (eql %h) b#hello         == b#h
barTakeWhile (lte 0xbe) x#deadbeef    == x#de
barTakeWhile (const 1) b#abc          == b#abc
```

### hexAlphabet

A bar containing the hexadecimal digits (0-9, a-f).

```sire
hexAlphabet              == b#0123456789abcdef
barLen hexAlphabet       == 16
barIdx 10 hexAlphabet    == %a
```

### hexChar

Converts a number (0-15) to its hexadecimal character representation.

```sire
hexChar 0     == 48
hexChar 9     == 57
hexChar 15    == %f
```

### byteToHex

Converts a byte to its two-character hexadecimal representation.

```sire
byteToHex 0      == b#00
byteToHex 255    == b#ff
byteToHex 171    == b#ab
```

### barToHex

Converts a bar to its hexadecimal string representation.

```sire
barToHex b#hello       == b#68656c6c6f
barToHex x#deadbeef    == b#deadbeef
barToHex b#{}          == b#{}
```

### readBarLit

Parses a bar literal from a Rex expression.

```sire
readBarLit '(b#hello) v2 id       == b#hello
readBarLit '(x#deadbeef) v2 id    == x#deadbeef
readBarLit '(b#{}) v2 id          == b#{}
```

### showBarLit

Converts a bar to its Rex literal representation.

```sire
showBarLit b#hello       == b#{hello}
showBarLit x#deadbeef    == x#{deadbeef}
showBarLit b#{}          == b#{}
```

### getHexBar

Parses a bar containing an ascii string as a hexadecimal number, passing the corresponding Nat to a continuation function or returning a fallback value.

```sire
getHexBar b#ff 0 natBar       == x#ff ; note the difference between b# and x#
getHexBar b#deadbeef 0 dec    == 0xdeadbeef
getHexBar b#xyz 42 id         == 42
```


### barLoadDecimal

Parses a decimal number from a bar.

```sire
barLoadDecimal b#123           == 123
barLoadDecimal b#0             == 0
barLoadDecimal b#9876543210    == 9876543210
```

### barShowDecimal

Converts a natural number to its decimal string representation as a bar.

```sire
barShowDecimal 123           == b#123
barShowDecimal 0             == b#0
barShowDecimal 9876543210    == b#9876543210
```

### barIsPrefixOf

Checks if a bar is a prefix of another bar at a given offset.

```sire
barIsPrefixOf b#he b#hello 0    == 1
barIsPrefixOf b#el b#hello 1    == 1
barIsPrefixOf b#lo b#hello 1    == 0
```

### barSubstringSearch

Finds all occurrences of a substring in a bar.

```sire
barSubstringSearch b#l b#hello    == [2 [3 0]]
barSubstringSearch b#o b#hello    == [4 0]
barSubstringSearch b#x b#hello    == 0
```

### barSpace

A bar containing a single space character.

```sire
barSpace             == b#{ }
barLen barSpace      == 1
barIdx 0 barSpace    == 32
```
