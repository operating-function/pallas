# Maybe

### NONE

Represents the absence of a value in the Maybe type.

```sire
NONE    == 0
```

### SOME

Wraps a value in the Maybe type.

```sire
SOME 5          == (0 5)
SOME b#hello    == (0 b#hello)
SOME []         == (0 [])
```

### maybeCase

Pattern matches on a Maybe value, providing cases for NONE and SOME.

```sire
maybeCase NONE 0 inc              == 0
maybeCase (SOME 5) 0 inc          == 6
maybeCase (SOME b#a) b#none id    == b#a
```

### maybe

Alias for `maybeCase`. Pattern matches on a Maybe value.

```sire
maybe 0 inc NONE              == 0
maybe 0 inc (SOME 5)          == 6
maybe b#none id (SOME b#a)    == b#a
```

### fromSome

Extracts the value from a SOME, or returns a default for NONE.

```sire
fromSome 0 (SOME 5)        == 5
fromSome 0 NONE            == 0
fromSome b#a (SOME b#b)    == b#b
```

### unpackSome

Extracts the value from a SOME, or crashes for NONE.

```sire
unpackSome (SOME 5)      == 5
unpackSome (SOME b#a)    == b#a
unpackSome NONE          ; crashes with "Unexpected NONE"
```

### isSome

Checks if a Maybe value is SOME.

```sire
isSome (SOME 5)     == 1
isSome (SOME [])    == 1
isSome NONE         == 0
```

### isNone

Checks if a Maybe value is NONE.

```sire
isNone NONE         == 1
isNone (SOME 5)     == 0
isNone (SOME [])    == 0
```

### fmapMaybe

Applies a function to the value inside a SOME, or returns NONE.

```sire
fmapMaybe (SOME 5) inc                == (0 6)
fmapMaybe (SOME b#b) (barWeld b#a)    == (0 b#ab)
fmapMaybe NONE inc                    == NONE
```

### maybeGuard

Returns NONE if the condition is false, otherwise returns the Maybe value.

```sire
maybeGuard TRUE (SOME 5)     == (0 5)
maybeGuard FALSE (SOME 5)    == NONE
maybeGuard TRUE NONE         == NONE
```

### maybeGuardNot

Returns NONE if the condition is true, otherwise returns the Maybe value.

```sire
maybeGuardNot FALSE (SOME 5)    == (0 5)
maybeGuardNot TRUE (SOME 5)     == NONE
maybeGuardNot FALSE NONE        == NONE
```

