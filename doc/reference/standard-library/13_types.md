# Types

### Type Tags

Encodings for primitive types.

```
pinTag=0
lawTag=1
appTag=2
natTag=3
barTag=4
rowTag=5
tabTag=6
cowTag=7
setTag=8
```

### typeTag

Returns the type tag of a given value.

```sire
typeTag 42            == 3 ; natTag
typeTag [1 2 3]       == 5 ; rowTag
typeTag #[a=1 b=2]    == 6 ; tabTag
```

### #datatype

Macro for defining new datatypes.

```sire
# datatype (Maybe a)
* NONE
* SOME x:a

# datatype (List a)
* NIL
* CONS head:a tail:(List a)

# datatype (Tree a)
* LEAF val:a
* NODE left:(Tree a) right:(Tree a)
```

### #datacase

Macro for pattern matching on datatypes.

```sire
# datacase (SOME 5)
* NONE     0
* (SOME x) x

# datacase myList
* NIL         0
* (CONS x xs) (add x (length xs))

# datacase myEither
* (LEFT err)  (handleError err)
* (RIGHT val) (processValue val)
```

### #record

Macro for defining record types.

```sire
# record (Point2D)
| POINT
* x : Nat
* y : Nat

# record (Person)
| PERSON
* name : Str
* age : Nat

# record (Config a)
| CONFIG
* enabled : Bit
* value : a
```

