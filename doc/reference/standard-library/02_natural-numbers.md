# Natural Numbers

::::warning
Type signatures are provisional and may contain errors.
::::

## Basic Operations

### isNat

```
(isNat n)
> n : a
> Bool
```

Checks if a value is a natural number.

```sire
isNat 5       == 1
isNat 0       == 1
isNat SOME    == 0
isNat b#hi    == 0
```

### toNat

```
(toNat n)
> n : a
> Nat
```

Converts a value to a natural number. If the input is already a natural number, it returns it unchanged.

```sire
toNat 5       == 5
toNat 0       == 0
toNat SOME    == 0 ; Non-numbers become 0
toNat b#hi    == 0
```

### times

```
(times f z n)
> f : (a > b)
> z : a
> n : Nat
> b
```

Applies a function `f` to an initial value `z`, `x` times.

```sire
times inc 0 3        == 3
times (mul 2) 1 4    == 16
times (add 2) 0 3    == 6
```

### inc

```
(inc n)
> n : Nat
> Nat
```

Increments a natural number by 1.

```sire
inc 0      == 1
inc 5      == 6
inc 255    == 256
```

### dec

```
(dec n)
> n : Nat
> Nat
```

Decrements a natural number by 1. Returns 0 if the input is 0.

```sire
dec 0      == 0
dec 1      == 0
dec 5      == 4
dec 256    == 255
```

### add

```
(add x y)
> x : Nat
> y : Nat
> Nat
```

Adds two natural numbers.

```sire
add 3 4      == 7
add 0 5      == 5
add 255 1    == 256
```

### sub

```
(sub x y)
> x : Nat
> y : Nat
> Nat
```

Subtracts one natural number from another. Returns 0 if the result would be negative.

```sire
sub 5 3      == 2
sub 3 5      == 0
sub 0 1      == 0
sub 256 1    == 255
```

### mod

```
(mod x y)
> x : Nat
> y : Nat
> Nat
```

Calculates the modulus of two natural numbers.

```sire
mod 7 3     == 1
mod 6 3     == 0
mod 5 10    == 5
mod 0 5     == 0
```

### mul

```
(mul x y)
> x : Nat
> y : Nat
> Nat
```

Multiplies two natural numbers.

```sire
mul 3 4      == 12
mul 0 5      == 0
mul 255 2    == 510
```

### div

```
(div x y)
> x : Nat
> y : Nat
> Nat
```

Performs integer division on two natural numbers. Division by zero returns 0.

```sire
div 6 3     == 2
div 7 3     == 2
div 5 10    == 0
div 5 0     == 0
```

### divMod

```
(divMod x y)
> x : Nat
> y : Nat
> SOME
```

Performs both division and modulus in one operation. Returns a tuple of (quotient, remainder).

```sire
divMod 7 3     == (0 2 1)
divMod 6 3     == (0 2 0)
divMod 5 10    == (0 0 5)
divMod 5 0     ; Division by zero, this will crash the REPL
```

### isOne

```
(isOne n)
> x : Nat
> Nat
```

Checks if a natural number is equal to 1.

```sire
isOne 1    == 1
isOne 0    == 0
isOne 2    == 0
```

## Bitwise Operations

### lsh

```
(lsh x y)
> x : Nat
> y : Nat
> Nat
```

Left-shifts a natural number by a given amount.

```sire
lsh 1 3    == 8     ; 1 << 3 = 8
lsh 5 2    == 20    ; 5 << 2 = 20
lsh 8 1    == 16    ; 8 << 1 = 16
```

### rsh

```
(rsh x y)
> x : Nat
> y : Nat
> Nat
```

Right-shifts a natural number by a given amount.

```sire
rsh 8 3     == 1    ; 8 >> 3 = 1
rsh 20 2    == 5    ; 20 >> 2 = 5
rsh 16 1    == 8    ; 16 >> 1 = 8
```

### con

```
(con x y)
> x : Nat
> y : Nat
> Nat
```

Performs a bitwise AND operation on two natural numbers.

```sire
con 5 3      == 1    ; 0101 & 0011 = 0001
con 12 10    == 8    ; 1100 & 1010 = 1000
con 15 7     == 7    ; 1111 & 0111 = 0111
```

### mix

```
(mix x y)
> x : Nat
> y : Nat
> Nat
```

Performs a bitwise XOR operation on two natural numbers.

```sire
mix 5 3      == 6    ; 0101 ^ 0011 = 0110
mix 12 10    == 6    ; 1100 ^ 1010 = 0110
mix 15 7     == 8    ; 1111 ^ 0111 = 1000
```

### dis

```
(dis x y)
> x : Nat
> y : Nat
> Nat
```

Performs a bitwise OR operation on two natural numbers.

```sire
dis 5 3      == 7     ; 0101 | 0011 = 0111
dis 12 10    == 14    ; 1100 | 1010 = 1110
dis 15 7     == 15    ; 1111 | 0111 = 1111
```

### pow

```
(pow b p)
> b : Nat
> p : Nat
> Nat
```

Raises a base to a power.

```sire
pow 2 3    == 8
pow 3 2    == 9
pow 5 0    == 1
pow 0 5    == 0
```

### bex

```
(bex p)
> p : Nat
> Nat
```

Calculates 2 raised to a given power.

```sire
bex 3    == 8     ; 2^3 = 8
bex 5    == 32    ; 2^5 = 32
bex 0    == 1     ; 2^0 = 1
```

### bix

```
(bix i n)
> i : Nat
> n : Nat
> Nat
```

Extracts a specific bit from a natural number.

```sire
bix 0 8    == 0    ; Least significant bit of 8 (1000) is 0
bix 3 8    == 1    ; 4th bit of 8 (1000) is 1
bix 2 7    == 1    ; 3rd bit of 7 (0111) is 1
```

### natEql

```
(natEql x y)
> x : Nat
> y : Nat
> Bool
```

Checks if two natural numbers are equal.

```sire
natEql 5 5     == 1    ; TRUE
natEql 3 5     == 0    ; FALSE
natEql 0 0     == 1    ; TRUE
```

### natCmp

```
(natCmp x y)
> x : Nat
> y : Nat
> Nat
```

Compares two natural numbers, returning an ordering result.

```sire
natCmp 3 5    == 0    ; LT
natCmp 5 3    == 2    ; GT
natCmp 4 4    == 1    ; EQ
```

## Advanced Bitwise Operations

### bitwise

```
(bitwise f x y)
> f : (Nat > Nat > Nat)
> x : Nat
> y : Nat
> Nat
```

Applies a bitwise operation to two natural numbers.

```sire
bitwise and 5 3    == 1    ; 0101 & 0011 = 0001
bitwise or  5 3    == 7    ; 0101 | 0011 = 0111
bitwise xor 5 3    == 6    ; 0101 ^ 0011 = 0110
```

### natFold

```
(natFold f z n)
> f : (a > Nat > a)
> z : a
> n : Nat
> a
```

Folds a function over the bits of a natural number.

```sire
natFold add 0 5     == 2    ; Sum of bits in 5 (0101)
natFold or  0 10    == 1    ; OR of all bits in 10 (1010)
```

### met

```
(met n)
> n : Nat
> Nat
```

Calculates the number of bits required to represent a natural number.

```sire
met 0      == 0
met 1      == 1
met 7      == 3
met 8      == 4
met 255    == 8
met 256    == 9
```

### popCount

```
(popCount n)
> n : Nat
> Nat
```

Counts the number of set bits in a natural number.

```sire
popCount 0     == 0
popCount 1     == 1
popCount 7     == 3    ; 0111 has 3 set bits
popCount 15    == 4    ; 1111 has 4 set bits
```

### trunc

```
(trunc w n)
> w : Nat
> n : Nat
> Nat
```

Truncates a natural number to a given bit width.

```sire
trunc 3 13    == 5
trunc 2 7     == 3     ; 7 (111) truncated to 2 bits is 3 (11)
trunc 4 15    == 15    ; 15 (1111) truncated to 4 bits is still 15
```

### bitSlice

```
(bitSlice o w n)
> o : Nat
> w : Nat
> n : Nat
> Nat
```

Extracts a slice of bits from a natural number.

```sire
bitSlice 0 3 13    == 5
bitSlice 1 2 13    == 2    ; Bits 1-2 of 13 (1101) is 2 (10)
bitSlice 2 2 13    == 3    ; Bits 2-3 of 13 (1101) is 3 (11)
```

### setBit

```
(setBit i n)
> i : Nat
> n : Nat
> Nat
```

Sets a specific bit in a natural number.

```sire
setBit 0 4    == 5
setBit 2 1    == 5    ; Set 3rd bit of 1 (001) to get 5 (101)
setBit 1 6    == 6    ; Setting already-set bit changes nothing
```

### clearBit

```
(clearBit i n)
> i : Nat
> n : Nat
> Nat
```

Clears a specific bit in a natural number.

```sire
clearBit 0 5    == 4
clearBit 2 7    == 3    ; Clear 3rd bit of 7 (111) to get 3 (011)
clearBit 1 4    == 4    ; Clearing already-clear bit changes nothing
```

### testBit

```
(testBit i n)
> i : Nat
> n : Nat
> Nat
```

Tests if a specific bit is set in a natural number.

```sire
testBit 0 5    == 1
testBit 1 5    == 0    ; 2nd bit of 5 (101) is not set
testBit 2 5    == 1    ; 3rd bit of 5 (101) is set
```

## Miscellaneous

### roundUp

````
(roundUp x y)
> x : Nat
> y : Nat
> Nat

Rounds a number up to the nearest multiple of another number.

```sire
roundUp 5 3    == 6
roundUp 6 3    == 6
roundUp 7 3    == 9
roundUp 0 3    == 0
````

### even

```
(even n)
> n : Nat
> Bool
```

Checks if a natural number is even.

```sire
even 0     == 1
even 1     == 0
even 2     == 1
even 15    == 0
```

### odd

```
(odd n)
> n : Nat
> Bool
```

Checks if a natural number is odd.

```sire
odd 0     == 0
odd 1     == 1
odd 2     == 0
odd 15    == 1
```
