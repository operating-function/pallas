= Opt
?? a
++ NONE
++ SOME a

> b > a>b > Opt a > b
= opt
? _ s SOME|x | s x
? n _ NONE   | n

> b > a>b > Opt a > b
= opt
? _ s SOME|x | s x
? n _ NONE   | n

> a > Opt a > a
= from-opt
? n NONE   | n
? _ SOME|s | x

> Opt Nat > Opt Nat > Opt Nat
= opt
? SOME|x SOME|y | SOME | add x y
? NONE   SOME|y | SOME y
? SOME|x NONE   | SOME x
? NONE   NONE   | NONE

> a>b>c > a*b > c
= [curry f x,y]
| f x y
