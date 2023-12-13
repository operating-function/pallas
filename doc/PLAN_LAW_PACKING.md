Bit-encoding of law bodies:

(1 v b) = 1 $v $b  ;  list node
x       = 0 $x     ;  list terminator

(0 f x) = 1 $f $x  ;  internal app node.
@ ≤ max = 00 VAR   ;  index into an fixed-size array of variables
*       = 01 CNS   ;  index into an fixed-size array of constants
