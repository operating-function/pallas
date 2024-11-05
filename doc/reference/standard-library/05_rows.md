# Rows

::::warning[Under Construction]
This page is under active development. It may contain bugs or incomplete descriptions.

TODO: Update with function and type signatures.
::::

This module defines operations on rows, which are [data-jetted](../../deeper/jets.md) arrays. Since PLAN is untyped, these are also used as the building blocks for tuples, records, and datatypes.

They are defined with `[]`:

```sire
arr=[10 64 42]

arr
[10 64 42]
```


### null

```
(null xs)
> xs : Row a
> Bool
```

Checks if a row is empty.

`null` returns `TRUE` if the given row is empty, and `FALSE` otherwise.

```sire
null []         == 1
null [1 2 3]    == 0
null [0]        == 0
```

### arity

```
(arity x)
> x : a
> Nat
```

Returns the arity of a function or row.

`arity` returns the number of arguments a function takes, or the number of elements in a row plus one.

```sire
arity add        == 2
arity [1 2 3]    == 4
arity []         == 1
```

### len

```
(len xs)
> xs : Row a
> Nat
```

Returns the length of a row.

`len` counts the number of elements in a given row.

```sire
len [1 2 3]    == 3
len []         == 0
len [9]        == 1
```

### idx

```
(idx i xs)
> i  : Nat
> xs : Row a
> a
```

Retrieves an element from a row at a specified index.

`idx` returns the element at the given index in the row. Indexing starts at 0.

```sire
idx 1 [10 20 30]    == 20
idx 0 [5 6 7]       == 5
idx 2 [1]           == 0 (out of bounds)
```

### last

```
(last xs)
> xs : Row a
> a
```

Retrieves an element from the end of a row.

```sire
last [10 20 30]    == 30
last [5 6 7]       == 7
last [] 2          == 0
```


### get

```
(get xs i)
> xs : Row a
> i  : Nat
> a
```

Retrieves an element from a row at a specified index.

`get` is an alias for `idx`. It returns the element at the given index in the row.

```sire
get [10 20 30] 1    == 20
get [5 6 7] 0       == 5
get [1] 2           == 0 (out of bounds)
```

### mut

```
(mut i v xs)
> i  : Nat
> v  : a
> xs : Row a
> Row a
```

Modifies an element in a row at a specified index.

`mut` returns a new row with the element at the given index replaced by the provided value.

```sire
mut 1 99 [10 20 30]    == [10 99 30]
mut 0 5 [1 2 3]        == [5 2 3]
mut 3 4 [1 2]          == [1 2 0 4]
```

### put

```
(put xs i v)
> xs : Row a
> i  : Nat
> v  : a
> Row a
```

Modifies an element in a row at a specified index.

`put` is an alias for `mut`. It returns a new row with the element at the given index replaced by the provided value.

```sire
put [10 20 30] 1 99    == [10 99 30]
put [1 2 3] 0 5        == [5 2 3]
put [1 2] 3 4          == [1 2 0 4]
```

### switch

```
(switch i d xs)
> i  : Nat
> d  : b
> xs : Row a
> a
```

Selects a value from a row based on an index, with a fallback value.

`switch` returns the element at the given index if it exists, otherwise it returns the fallback value.

```sire
switch 1 0 [10 20 30]    == 20
switch 3 0 [10 20 30]    == 0 (fallback)
switch 0 99 []           == 99 (fallback)
```

### c0, c1, c2, c3, c4, c5, c6, c7, c8, c9

Creates a row constructor of the specified arity.

These functions create row constructors of arity 0 to 9 respectively.

```sire
c3 1 2 3      == [3 2 1]
c0            == []
c2 b#a b#b    == [b#b b#a]
```

### v0, v1, v2, v3, v4, v5, v6, v7, v8, v9

Creates a row of the specified arity.

These functions create rows of arity 0 to 9 respectively, with the arguments in reverse order.

```sire
v3 1 2 3      == [1 2 3]
v0            == []
v2 b#a b#b    == [b#a b#b]
```

### isRow

```
(isRow x)
> x : a
> Bool
```

Checks if a value is a row.

`isRow` returns `TRUE` if the given value is a row, and `FALSE` otherwise.

```sire
isRow [1 2 3]    == 1
isRow []         == 1
isRow 3          == 0
```

### foldr

```
(foldr f xs)
> f  : (a > b)
> xs : Row a
> b
```

Folds a row from right to left.

foldr applies a binary function to a starting value (from the right) and all elements of the row, going from right to left.

```sire
foldr sub 0 [1 2 3]    == 1
foldr v2 [] [1 2 3]    == [1 [2 [3 []]]]
foldr add 0 []         == 0
```

### foldl

```
(foldl f xs)
> f  : (a > b)
> xs : Row a
> b
```

Folds a row from left to right.

foldl applies a binary function to a starting value (from the left) and all elements of the row, going from left to right.

```sire
foldl sub 0 [1 2 3]    == 0
foldl v2 [] [1 2 3]    == [[[[] 1] 2] 3]
foldl add 0 [1]        == 1
```

### foldr1

```sire
(foldr1 f xs)
> f  : (a > b)
> xs : Row a
> b
```

Folds a non-empty row from right to left using the last element as the initial accumulator.

```sire
foldr1 sub [1 2 3 10]        == 0
foldr1 strWeld [%a %b %c]    == %abc
foldr1 max [1 5 3 2]         == 5
```

### unfoldr

```
(unfoldr f s)
> f : (a > b)
> s : a
> a
```

Builds a row from a seed value using a function that returns either a value and a new seed, or nothing.

```sire
unfoldr n&(if (gth n 0) (SOME [n dec-n]) NONE) 5    == [5 4 3 2 1]
unfoldr n&(if (lth n 3) (SOME [n inc-n]) NONE) 0    == [0 1 2]
unfoldr _&NONE 0                                    == []
```

### strictRow

```
(strictRow x)
> x : a
> Row a
```

Forces evaluation of all elements in a row.

```sire
strictRow [1 2 3]                  == [1 2 3] ; ensures all elements are evaluated
strictRow (map (mul 2) [1 2 3])    == [2 4 6] ; forces computation
strictRow []                       == []
```

### weld

```
(weld xs ys)
> xs : Row a
> ys : Row a
> Row a
```

Concatenates two rows.

`weld` combines two rows into a single row, with the elements of the first row followed by the elements of the second row.

```sire
weld [1 2] [3 4]    == [1 2 3 4]
weld [] [5 6]       == [5 6]
weld [1 2] []       == [1 2]
```

### gen

```
(gen l f)
> l : Nat
> f : (Nat > a)
> Row a
```

Generates a row based on a function and a length.

`gen` creates a row of the specified length, where each element is the result of applying the given function to its index.

```sire
gen 3 (add 2)    == [2 3 4]
gen 4 id         == [0 1 2 3]
gen 0 | mul 2    == []
```

### fst

```
(fst xs)
> xs : Row a
> a
```

Returns the first element of a row.

`fst` retrieves the leftmost element of a given row.

```sire
fst [1 2 3]    == 1
fst [9]        == 9
fst []         == 0
```

### snd

```
(snd xs)
> xs : Row a
> a
```

Returns the second element of a row.

`snd` retrieves the second element of a given row. If the row has fewer than two elements, it returns 0.

```sire
snd [1 2 3]    == 2
snd [9]        == 0
snd []         == 0
```

### thr

```
(thr xs)
> xs : Row a
> a
```

Returns the third element of a row.

`thr` retrieves the third element of a given row. If the row has fewer than three elements, it returns 0.

```sire
thr [1 2 3 4]    == 3
thr [1 2]        == 0
thr []           == 0
```

### map

```
(map f xs)
> f  : (a > b)
> xs : Row a
> Row b
```

Applies a function to each element of a row.

`map` creates a new row by applying the given function to each element of the input row.

```sire
map (mul 2) [1 2 3]            == [2 4 6]
map fst [[1 2] [3 4] [5 6]]    == [1 3 5]
map (add 1) []                 == []
```

### foreach

```
(foreach xs f)
> xs : Row a
> f  : (a > b)
> Row b
```

Alias for `map` with arguments reversed.

`foreach` is an alias for `map`. It applies a function to each element of a row.

```sire
foreach [1 2 3] (mul 2)            == [2 4 6]
foreach [[1 2] [3 4] [5 6]] fst    == [1 3 5]
foreach [] (add 1)                 == []
```

### rev

```
(rev xs)
> xs : Row a
> Row a
```

Reverses a row.

`rev` creates a new row with the elements of the input row in reverse order.

```sire
rev [1 2 3]    == [3 2 1]
rev [9]        == [9]
rev []         == []
```

### curry

```
(curry f x y)
> f : (Row a > b)
> x : a
> y : a
> a
```

Converts a function that takes a row to a curried function.

curry takes a function that expects a row as its argument and returns a function that takes two arguments separately.

```
See the function definition in sire/sire_05_row.sire.
```

### uncurry

```
(uncurry f xs)
> f  : (a > a)
> xs : Row a
> f (Row a)
```

Converts a curried function to a function that takes a row.

uncurry takes a function that expects two separate arguments and returns a function that takes a row of two elements.

```sire
uncurry add [1 2]    == 3
uncurry sub [5 3]    == 2
uncurry v2 [1 2]     == [1 2]
```

### rowCons

```
(rowCons e xs)
> e  : a
> xs : Row a
> Row a
```

Prepends an element to a row.

`rowCons` creates a new row with the given element as the first element, followed by all elements of the input row.

```sire
rowCons 1 [2 3]    == [1 2 3]
rowCons b#a []     == [b#a]
rowCons 0 [1]      == [0 1]
```

### rowSnoc

```
(rowSnoc xs e)
> xs : Row a
> e  : a
> Row a
```

Appends an element to a row.

`rowSnoc` creates a new row with all elements of the input row, followed by the given element as the last element.

```sire
rowSnoc [1 2] 3    == [1 2 3]
rowSnoc [] b#a     == [b#a]
rowSnoc [0] 1      == [0 1]
```

### rowApply

```
(rowApply f xs)
> f  : (a > b)
> xs : Row a
> b
```

Applies a function to a row of arguments.

`rowApply` takes a function and a row of arguments, and applies the function to those arguments.

```sire
rowApply add [2 3]    == 5
rowApply gte [3 4]    == 0
rowApply lte [3 4]    == 1
```

### rowRepel

```
(rowRepel f xs)
> f  : (a > b)
> xs : Row a
> b
```

Applies a function to a row of arguments in reverse order.

`rowRepel` takes a function and a row of arguments, and applies the function to those arguments in reverse order.

```sire
rowRepel add [2 3]    == 5
rowRepel gte [3 4]    == 1
rowRepel lte [3 4]    == 0
```

### slash

```
(slash xs s e)
> xs : Row a
> s  : Nat
> e  : Nat
> a
```

Extracts a slice from a row, from index `s` to index `e`, padding with zeros if necessary.

```sire
slash [1 2 3 4 5] 1 3    == [2 3]
slash [1 2 3] 0 5        == [1 2 3 0 0]
slash [1 2 3] 2 2        == []
```

### slice

```
(slice xs s e)
> xs : Row a
> s : Nat
> e : Nat
> a
```

Similar to `slash`, but doesn't pad with zeros. It returns a slice from index `s` up to (but not including) index `e`.

```sire
slice [1 2 3 4 5] 1 3    == [2 3]
slice [1 2 3] 0 5        == [1 2 3]
slice [1 2 3] 2 2        == []
```

### chunks

```
(chunks n xs)
> n  : Nat
> xs : Row a
> Row a
```

Splits a row into chunks of a specified size.

```sire
chunks 2 [1 2 3 4 5]    == [[1 2] [3 4] [5]]
chunks 3 [1 2 3 4]      == [[1 2 3] [4]]
chunks 5 [1 2 3]        == [[1 2 3]]
```

### rep

```
(rep x n)
> x : a
> n : Nat
> Row a
```

Creates a row by repeating a value a specified number of times.

```sire
rep 3 2         == [3 3]
rep b#aaab 3    == [b#aaab b#aaab b#aaab]
rep [] 2        == [[] []]
```

### rowIndexed

```
(rowIndexed xs)
> xs : Row a
> Row (Nat, a)
```

Creates a row of pairs, where each pair contains the index and the corresponding element from the input row.

```sire
rowIndexed [10 20 30]       == [[0 10] [1 20] [2 30]]
rowIndexed [b#aba b#bab]    == [[0 b#aba] [1 b#bab]]
rowIndexed []               == []
```

### findIdx

```
(findIdx f xs d k)
> f  : (a > Bool)
> xs : Row a
> d  : b
> k  : (a > b)
> a
```

Finds the index of the first element in a row that satisfies a predicate function.

```sire
findIdx (lte 5) [1 3 5 7 9] 0 id              == 2
findIdx (lte 10) [1 3 5 7 9] 0 id             == 0
findIdx even [1 3 5 7] {not found} showNat    == {not found}
```

### elemIdx

```
(elemIdx e xs d k)
> e  : a
> xs : Row a
> d  : a
> k  : a
> a
```

Finds the index of the first occurrence of a specific element in a row.

```sire
elemIdx 5 [1 3 5 7 5] b#{not found} id        == 2
elemIdx b#a [b#b b#a b#c] b#{not found} id    == 1
elemIdx 4 [1 2 3] b#{not found} id            == b#{not found}
```

### has

```
(has e xs)
> e  : a
> xs : Row a
> Bool
```

Checks if a row contains a specific element.

```sire
has 3 [1 2 3 4 5]      == 1
has b#a [b#b b#c]      == 0
has [] [[1] [2] []]    == 1
```

### rowAnd

```
(rowAnd xs)
> xs : Row Bool
> Bool
```

Performs a logical AND operation on all elements of a row.

```sire
rowAnd [TRUE TRUE FALSE]    == 0
rowAnd [TRUE TRUE TRUE]     == 1
rowAnd []                   == 1
```

### rowOr

```
(rowOr xs)
> xs : Row Bool
> Bool
```

Performs a logical OR operation on all elements of a row.

```sire
rowOr [FALSE FALSE TRUE]     == 1
rowOr [FALSE FALSE FALSE]    == 0
rowOr []                     == 0
```

### sum

```
(sum xs)
> xs : Row Nat
> Nat
```

Calculates the sum of all elements in a row.

```sire
sum [1 2 3 4 5]          == 15
sum [10 (sub 10 3) 3]    == 20
sum []                   == 0
```

### sumOf

```
(sumOf f xs)
> f  : (a > Nat)
> xs : Row a
> Nat
```

Applies a function to each element of a row and then calculates the sum of the results.

```sire
sumOf (mul 2) [1 2 3 4]    == 20
sumOf (pow 2) [1 2 3]      == 14
sumOf id []                == 0
```

### all

```
(all f xs)
> f  : (a > Bool)
> xs : Row a
> Bool
```

Checks if all elements in a row satisfy a given predicate.

```sire
all even [2 4 6 8]     == 1
all (lte 0) [1 2 3]    == 1
all id []              == 1
```

### any

```
(any f xs)
> f  : (a > Bool)
> xs : Row a
> Bool
```

Checks if any element in a row satisfies a given predicate.

```sire
any odd [2 4 5 8]        == 1
any (gte 0) [1 2 3 4]    == 0
any id []                == 0
```

### zip

```
(zip xs ys)
> xs : Row a
> ys : Row b
> Row (a, b)
```

Combines two rows into a row of pairs.

```sire
zip [1 2 3] [b#a b#b b#c]    == [[1 b#a] [2 b#b] [3 b#c]]
zip [1 2] [b#a b#b b#c]      == [[1 b#a] [2 b#b]]
zip [] [1 2 3]               == []
```

### zipWith

```
(zipWith f xs y)
> f : (a > b > c)
> xs : Row a
> y : Row b
> Row c
```

Combines two rows using a given function.

```sire
zipWith add [1 2 3] [4 5 6]                == [5 7 9]
zipWith mul [1 2 3] [1 2 3]                == [1 4 9]
zipWith zip [[1 2] [1 2]] [[3 4] [3 4]]    == [[[1 3] [2 4]] [[1 3] [2 4]]]
```

### cat

```
(cat x)
> xs : Row (Row a)
> Row a
```

Concatenates a row of rows into a single row.

```sire
cat [[1 2 3 4] [3 4] [5]]    == [1 2 3 4 5]
cat [[] [1 2] [3]]           == [1 2 3]
cat []                       == []
```

### catMap

```
(catMap f x)
> f  : (a > Row b)
> xs : Row a
> Row b
```

Applies a function to each element of a row and concatenates the results.

```sire
catMap (rep 2) [1 2 3]     == [2 2 2 2 2 2]
catMap id [[1 2] [3 4]]    == [1 2 3 4]
```

### take

```
(take n x)
> n  : Nat
> xs : Row a
> Row a
```

Returns the first n elements of a row.

```sire
take 3 [1 2 3 4 5]    == [1 2 3]
take 2 [b#a b#b]      == [b#a b#b]
take 5 [1 2 3]        == [1 2 3]
```

### drop

```
(drop n xs)
> n  : Nat
> xs : Row a
> Row a
```

Removes the first n elements from a row.

```sire
drop 2 [1 2 3 4 5]          == [3 4 5]
drop 3 [b#a b#b b#c b#d]    == [b#d]
drop 5 [1 2 3]              == []
```

### rev

```
(rev xs)
> xs : Row a
> Row a
```

Reverses the order of elements in a row.

```sire
rev [1 2 3 4 5]          == [5 4 3 2 1]
rev [b#a b#b b#c b#d]    == [b#d b#c b#b b#a]
rev []                   == []
```

### span

```
(span f xs)
> f  : (a > Bool)
> xs : Row a
> Row (Row a)
```

Splits a row into two parts: the longest prefix that satisfies a predicate and the rest.

```sire
span (gte 3) [1 2 3 4 1 2 3 4]    == [[1 2 3] [4 1 2 3 4]]
span even [2 4 6 7 8 9]           == [[2 4 6] [7 8 9]]
span FALSE [1 2 3]                == [[] [1 2 3]]
```

### splitAt

```
(splitAt i xs)
> i  : Nat
> xs : Row a
> Row (Row a)
```

Splits a row at a given index.

```sire
splitAt 3 [1 2 3 4 5]    == [[1 2 3] [4 5]]
splitAt 0 [1 2 3]        == [[] [1 2 3]]
splitAt 5 [1 2 3]        == [[1 2 3] []]
```

### intersperse

```
(intersperse e xs)
> e  : a
> xs : Row a
> Row a
```

Intersperses an element between every element of a row.

```sire
intersperse 0 [1 2 3]    == [1 0 2 0 3]
intersperse b#a [b#b]    == [b#b]
intersperse 0 []         == []
```

### insert

```
(insert i e xs)
> i  : Nat
> e  : a
> xs : Row a
> Row a
```

Inserts an element at a specified index in a row.

```sire
insert 1 b#x [b#a b#b b#c]    == [b#a b#x b#b b#c]
insert 0 1 [1 2 3]            == [1 1 2 3]
insert 3 4 [1 2 3]            == [1 2 3 4]
```

## Sorting and Filtering

### sort

```
(sort xs)
> xs : Row a
> Row a
```

Sorts a row in ascending order.

```sire
sort [3 1 4 1 5]      == [1 1 3 4 5]
sort [b#c b#a b#b]    == [b#a b#b b#c]
sort []               == []
```

### sortBy

```
(sortBy f xs)
> f  : (a > Nat)
> xs : Row a
> Row a
```

Sorts a row using a custom comparison function.

```sire
sortBy (flip cmp) [1 3 2]      == [3 2 1]
sortBy (cmp) [[1 2] [] [1]]    == [[] [1] [1 2]]
```

### sortOn

```
(sortOn f xs)
> f  : (a > b)
> xs : Row a
> Row b
```

Sorts a row by applying a function to each element before comparing.

```sire
sortOn (even) [1 3 2]             == [3 1 2]
sortOn len [[1 2] [3] [4 5 6]]    == [[3] [1 2] [4 5 6]]
```

### sortUniq

```
(sortUniq xs)
> xs : Row a
> Row a
```

Sorts a row and removes duplicate elements.

```sire
sortUniq [3 1 4 1 5 3]    == [1 3 4 5]
sortUniq [b#a b#b b#a]    == [b#a b#b]
sortUniq []               == []
```

### filter

```
(filter f xs)
> f  : (a > Bool)
> xs : Row a
> Row a
```

Keeps only the elements of a row that satisfy a predicate.

```sire
filter even [1 2 3 4 5]               == [2 4]
filter (neq b#a) [b#a b#b b#a b#c]    == [b#b b#c]
filter (const TRUE) [1 2 3]           == [1 2 3]
```

### delete

```
(delete e xs)
> e  : a
> xs : Row a
> Row a
```

Removes all occurrences of a value from a row.

```sire
delete 3 [1 2 3 4 3 5]      == [1 2 4 5]
delete b#a [b#a b#b b#a]    == [b#b]
delete 42 [1 2 3]           == [1 2 3]
```

### findIdxMany

```
(findIdxMany f xs)
> f  : (a > Bool)
> xs : Row a
> List a
```

Finds all indices where a predicate is satisfied.

```sire
findIdxMany even [1 2 3 4 5 6]         == [1 [3 [5 0]]]
findIdxMany (eql b#a) [b#a b#b b#a]    == [0 [2 0]]
findIdxMany (const FALSE) [1 2 3]      == 0
```

### elemIdxMany

```
(elemIdxMany e xs)
> e  : a
> xs : Row a
> Either (List a) Nat
```

Finds all indices where a specific element occurs.

```sire
elemIdxMany 3 [1 2 3 4 3 5]      == [2 [4 0]]
elemIdxMany b#a [b#a b#b b#a]    == [0 [2 0]]
elemIdxMany 42 [1 2 3]           == 0
```

