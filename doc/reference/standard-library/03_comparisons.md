# Comparisons

::::warning
Type signatures are provisional and may contain errors.
::::

### LT

```
(LT)
> Nat
```

Represents "less than" in ordering comparisons.

```sire
LT == 0
```

### EQ

```
(EQ)
> Nat
```

Represents "equal to" in ordering comparisons.

```sire
EQ == 1
```

### GT

```
(GT)
> Nat
```

Represents "greater than" in ordering comparisons.

```sire
GT == 2
```

### ordWeld

```
(ordWeld x y)
> x : Nat
> y : Nat
> Nat
```

Combines two ordering results, giving precedence to the first non-EQ result.

```sire
ordWeld LT EQ    == 0    ; LT
ordWeld EQ GT    == 2    ; GT
ordWeld EQ EQ    == 1    ; EQ
ordWeld LT GT    == 0    ; LT
```

### isZero

```
(isZero x)
> x : Nat
> Bool
```

Checks if a value is zero.

```sire
isZero 0      == 1
isZero 1      == 0
isZero 100    == 0
```

### isOne

```
(isOne x)
> x : Nat
> Bool
```

Checks if a value is one.

```sire
isOne 1      == 1
isOne 100    == 0
```

### cmp

```
(cmp x y)
> x : a
> y : a
> Nat
```

Compares two values, returning LT, EQ, or GT.

```sire
cmp 1 2            == LT
cmp 2 2            == EQ
cmp 3 2            == GT
cmp [1 2] [1 3]    == LT
cmp [1 2] [1 2]    == EQ
```

### eql

```
(eql x y)
> x : a
> y : a
> Bool
```

Checks if two values are equal.

```sire
eql 1 1            == 1
eql 1 2            == 0
eql [1 2] [1 2]    == 1
eql [1 2] [1 3]    == 0
```

### neq

```
(neq x y)
> x : a
> y : a
> Bool
```

Checks if two values are not equal.

```sire
neq 1 2            == 1
neq 1 1            == 0
neq [1 2] [1 3]    == 1
neq [1 2] [1 2]    == 0
```

### lth

```
(lth x y)
> x : a
> y : a
> Bool
```

Checks if the first value is less than the second.

```sire
lth 1 2    == 1
lth 2 1    == 0
lth 1 1    == 0
```

### lte

```
(lte x y)
> x : a
> y : a
> Bool
```

Checks if the first value is less than or equal to the second.

```sire
lte 1 2    == 1
lte 1 1    == 1
lte 2 1    == 0
```

### gth

```
(gth x y)
> x : a
> y : a
> Bool
```

Checks if the first value is greater than the second.

```sire
gth 2 1    == 1
gth 1 2    == 0
gth 1 1    == 0
```

### gte

```
(gte x y)
> x : a
> y : a
> Bool
```

Checks if the first value is greater than or equal to the second.

```sire
gte 2 1    == 1
gte 1 1    == 1
gte 1 2    == 0
```

### min

```
(min x y)
> x : a
> y : a
> a
```

Returns the minimum of two values.

```sire
min 1 2    == 1
min 2 1    == 1
min 1 1    == 1
```

### max

```
(max x y)
> x : a
> y : a
> a
```

Returns the maximum of two values.

```sire
max 1 2    == 2
max 2 1    == 2
max 1 1    == 1
```
