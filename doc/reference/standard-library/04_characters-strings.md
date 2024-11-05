# Characters and Strings

::::warning[TODO]
TODO: rectify `ord` and `chr` with REPL formatting; should results format as in the REPL?
::::

## Character Functions

### ord

Converts a digit character to its numeric value. For non-digit characters, it returns the ASCII value minus 48.

```sire
ord {1}    == 1
ord {A}    == 17
ord {a}    == 49
```

### chr

Converts a number to its corresponding ASCII character by adding 48.

```sire
chr 48    == {0}
chr 65    == {A}
chr 97    == {a}
```

### isDigit

Checks if a character is a digit (0-9).

```sire
isDigit {0}    == 1
isDigit {9}    == 1
isDigit {a}    == 0
```

### isHexDigit

Checks if a character is a hexadecimal digit (0-9, a-f, A-F).

```sire
isHexDigit {0}    == 1
isHexDigit {F}    == 1
isHexDigit {g}    == 0
```

### isUpper

Checks if a character is uppercase.

```sire
isUpper {A}    == 1
isUpper {a}    == 0
isUpper {0}    == 0
```

### isLower

Checks if a character is lowercase.

```sire
isLower {a}    == 1
isLower {A}    == 0
isLower {0}    == 0
```

### isAlpha

Checks if a character is alphabetic (a-z, A-Z).

```sire
isAlpha {a}    == 1
isAlpha {Z}    == 1
isAlpha {0}    == 0
```

### isPrint

Checks if a character is printable (space through tilde).

```sire
isPrint { }     == 1
isPrint {~}     == 1
isPrint {a}     == 1
isPrint 0       == 0
```

### isAlphaNum

Checks if a character is alphanumeric (a-z, A-Z, 0-9).

```sire
isAlphaNum {a}    == 1
isAlphaNum {Z}    == 1
isAlphaNum {0}    == 1
isAlphaNum {!}    == 0
```

### toLower

Converts a character to lowercase.

```sire
toLower {A}    == {a}
toLower {a}    == {a}
toLower {0}    == 48
```

### toUpper

Converts a character to uppercase.

```sire
toUpper {a}    == {A}
toUpper {A}    == {A}
toUpper {0}    == 48
```

## Special Characters

### newlineChar

The ASCII code for newline (10).

```sire
newlineChar == 10
```

### tabChar

The ASCII code for tab (9).

```sire
tabChar == 9
```

### spaceChar

The ASCII code for space (32).

```sire
spaceChar == 32
```

## Number to String Conversion

### listDigits

Converts a number to a list of digit characters.

```sire
listDigits 0     == [48 0]
listDigits 123   == [49 [50 [51 0]]]
```

### digits

Converts a number to a row of digit characters.

```sire
digits 0     == [48]
digits 123   == [49 50 51]
```

## String Functions

### strLen

Returns the length of a string.

```sire
strLen {}        == 0
strLen {abc}     == 3
strLen {hello}   == 5
```

### strWeld

Concatenates two strings.

```sire
strWeld {hello} { world}    == {hello world}
strWeld {} {abc}            == {abc}
strWeld {abc} {}            == {abc}
```

### strCat

Concatenates a row of strings.

```sire
strCat [{a} {b} {c}]    == {abc}
strCat [{} {abc} {}]    == {abc}
```

### strToList

::::warning[TODO]
TODO: REPL formatting?
::::

Converts a string to a list of character codes.

```sire
strToList {abc}    == [97 [98 [99 0]]]
strToList {}       == 0
```

### strFromList

::::warning[TODO]
TODO: REPL formatting?
::::

Converts a list of character codes to a string.

```sire
strFromList [97 [98 [99 0]]]    == {abc}
strFromList 0                   == {}
```

### explode

::::warning[TODO]
TODO: REPL formatting?
::::

Converts a string to a row of character codes.

```sire
explode {abc}    == [97 98 99]
explode {}       == []
```

### implode

Converts a row of character codes to a string.

```sire
implode [97 98 99]    == {abc}
implode []            == {}
```

### strToUpper

Converts a string to uppercase.

```sire
strToUpper {Hello}     == {HELLO}
strToUpper {ABC123}    == {ABC123}
```

### strToLower

Converts a string to lowercase.

```sire
strToLower {Hello}     == {hello}
strToLower {ABC123}    == {abc123}
```

### strCapitalize

Capitalizes the first character of a string.

```sire
strCapitalize {hello}    == {Hello}
strCapitalize {HELLO}    == {HELLO}
strCapitalize {}         == {}
```

### strIsCapitalized

Checks if the first character of a string is uppercase.

```sire
strIsCapitalized {Hello}    == 1
strIsCapitalized {hello}    == 0
strIsCapitalized {}         == 0
```

### strMap

Applies a function to every character in a string.

```sire
strMap toUpper {Hello}      == {HELLO}
strMap (add 1) {ABC}        == {BCD}
strMap (const {x}) {abc}    == {xxx}
```

## String Parsing Functions

### isDecimalLit

Checks if a string is a valid decimal literal.

```sire
isDecimalLit {123}      == 1
isDecimalLit {12_34}    == 1
isDecimalLit {12.34}    == 0
isDecimalLit {abc}      == 0
```

### loadDecimal

Parses a decimal literal string into a number.

```sire
loadDecimal {123}      == 123
loadDecimal {12_34}    == 1234
loadDecimal {0}        == 0
```

### isHexLit

Checks if a string is a valid hexadecimal literal (starting with "0x").

```sire
isHexLit {0xff}       == 1
isHexLit {0xAB_CD}    == 1
isHexLit {ff}         == 0
isHexLit {0xGG}       == 0
```

### loadHexLit

Parses a hexadecimal literal string into a number.

```sire
loadHexLit {0xff}     == 255
loadHexLit {0xA_B}    == 171
loadHexLit {0x0}      == 0
```

### loadKeyWord

Parses a string as either a decimal literal, hexadecimal literal, or returns the string itself.

```sire
loadKeyWord {123}     == 123
loadKeyWord {0xff}    == 255
loadKeyWord {abc}     == {abc}
```
