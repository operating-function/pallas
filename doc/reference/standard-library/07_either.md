# Either

### LEFT

Constructs a Left value in an Either type.

```sire
LEFT 1          == [0 1]
LEFT b#hello    == [0 b#hello]
LEFT NIL        == [0 0]
```

### RIGHT

Constructs a Right value in an Either type.

```sire
RIGHT 1          == [1 1]
RIGHT b#hello    == [1 b#hello]
RIGHT NIL        == [1 0]
```

### fromLeft

Extracts the value from a Left, or returns a default value for a Right.

```sire
fromLeft 0 [0 2]          == 2
fromLeft 0 ~[0 2]         == [2 0]
fromLeft 1 (LEFT b#b)     == b#b
fromLeft 1 (RIGHT b#b)    == (1 b#b)
```

### unpackLeft

Extracts the value from a Left, or crashes for a Right.

```sire
unpackLeft (LEFT 1)      == 1
unpackLeft (LEFT b#a)    == b#a
unpackLeft (RIGHT 1)     ; crashes with "Unexpected RIGHT"
```

### fromRight

Extracts the value from a Right, or returns a default value for a Left.

```sire
fromRight 0 (RIGHT 1)               == 1
fromRight 0 (LEFT 1)                == (0 1)
fromRight natBar (RIGHT b#right)    == b#right
fromRight natBar (LEFT {left})      == b#left
```

### unpackRight

Extracts the value from a Right, or crashes for a Left.

```sire
unpackRight (RIGHT 1)      == 1
unpackRight (RIGHT b#a)    == b#a
unpackRight (LEFT 1)       ; crashes with "Unexpected LEFT"
```

### eitherCase

Pattern matches on an Either value, applying the appropriate function.

```sire
eitherCase [1 0] (add 1) (add 3)                     == 3
eitherCase (RIGHT 1) (add 1) (add 3)                 == 3
eitherCase [0 b#a] (const b#left) (const b#right)    == b#left
```

### eitherOpen

Pattern matches on an Either value, returning the value for a Right, or applying a function for a Left.

```sire
eitherOpen [0 2] (add 1)                 == [0 2]
eitherOpen [1 2] (add 1)                 == 3
eitherOpen (LEFT b#a) (const b#left)     == [0 b#a]
eitherOpen (RIGHT b#a) (const b#left)    == b#left
```

### eitherOpenLeft

Pattern matches on an Either value, returning the value for a Left, or applying a function for a Right.

```sire
eitherOpenLeft [0 2] (add 1)                  == 3
eitherOpenLeft [1 2] (add 1)                  == [1 2]
eitherOpenLeft (LEFT b#a) (const b#right)     == b#right
eitherOpenLeft (RIGHT b#a) (const b#right)    == [1 b#a]
```

### eitherCaseLeft

Pattern matches on an Either value, applying the appropriate function (opposite order of eitherCase).

```sire
eitherCaseLeft [1 0] (add 1) (add 3)                      == 1
eitherCaseLeft (RIGHT 1) (add 1) (add 3)                  == 2
eitherCaseLeft [0 b#a] (const b#left) (const b#right)     == b#right
```

### eitherGetRight

Extracts the value from a Right, or applies a function to the Left value.

```sire
eitherGetRight (RIGHT 1) (add 1)             == 2
eitherGetRight (LEFT 1) (add 1)              == [0 1]
eitherGetRight (RIGHT b#a) (const b#left)    == b#left
```

### eitherGetLeft

Extracts the value from a Left, or applies a function to the Right value.

```sire
eitherGetLeft (LEFT 1) (add 1)              == 2
eitherGetLeft (RIGHT 1) (add 1)             == [1 1]
eitherGetLeft (LEFT b#a) (const b#right)    == b#right
```

### eitherMap

Applies a function to the value in a Right, leaving a Left unchanged.

```sire
eitherMap (add 2) (RIGHT 1)                     == (1 3)
eitherMap (add 2) (LEFT 1)                      == (0 1)
eitherMap (strWeld {hello_}) (RIGHT {world})    == (1 {hello_world})
```

### eitherBind

Applies a function that returns an Either to the value in a Right, leaving a Left unchanged.

```sire
eitherBind (RIGHT 1) (x & if (even x) (RIGHT (mul x 2)) (LEFT x))             == [0 1]
eitherBind (RIGHT 2) (x & if (even x) (RIGHT (mul x 2)) (LEFT x))             == [1 4]
eitherBind (RIGHT b#bye) (x & if (eql x b#hello) (RIGHT b#world) (LEFT x))    == [0 b#bye]
```

### partition

Separates a row of Either values into two rows: one for Left values and one for Right values.

```sire
partition [[0 0] [1 1] [1 2] [0 3]]               == [[0 3] [1 2]]
partition [LEFT-1 RIGHT-2 LEFT-3 RIGHT-4]         == [[1 3] [2 4]]
partition [(RIGHT b#a) (LEFT b#b) (RIGHT b#c)]    == [[b#b] [b#a b#c]]
partition []                                      == [[] []]
```

