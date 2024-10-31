# Definition

Like [Urbit's Nock](https://developers.urbit.org/reference/nock/definition), PLAN is a combinator interpreter. It is defined by the following pseudocode:

```
Every PLAN vaue is either a pin x:<i>, a law x:{n a b}, an app x:(f g), a
nat x:@, or a black hole x:<>.  Black holes only exist during evaluation.

(o <- x) mutates o in place, replacing it's value with x.

Run F(x) to normalize a value.

E(o:@)     = o                          | F(o) =
E(o:<x>)   = o                          |     E(o)
E(o:(f x)) =                            |     when o:(f x)
    E(f)                                |          F(f); F(x)
    when A(f)=1                         |     o
        o <- X(o,o)                     |
        E(o)                            | N(o) = E(o); if o:@ then o else 0
    o                                   |
E(o:{n a b}) =                          | I(f, (e x), 0) = x
    if a!=0 then o else                 | I(f, e,     0) = e
        o <- <>                         | I(f, (e x), n) = I(f, e, n-1)
        o <- R(0,o,b)                   | I(f, e,     n) = f
        E(o)                            |
                                        | A((f x))     = A(f)-1
X((f x), e)         = X(f,e)            | A(<p>)       = A(p)
X(<p>, e)           = X(p,e)            | A({n a b})   = a
X({n a b}, e)       = R(a,e,b)          | A(n:@)       = I(1, (3 5 3), n)
X(0, (_ n a b))     = {N(n) N(a) F(b)}  |
X(1, (_ p l a n x)) = P(p,l,a,n,E(x))   | R(n,e,b:@) | b≤n = I(x,e,(n-b))
X(2, (_ z p x))     = C(z,p,N(x))       | R(n,e,(0 f x))   = (R(n,e,f) R(n,e,x))
X(3, (_ x))         = N(x)+1            | R(n,e,(1 v b))   = L(n,e,v,b)
X(4, (_ x))         = <F(x)>            | R(n,e,(2 x))     = x
                                        | R(n,e,x)         = x
C(z,p,n) = if n=0 then z else (p (n-1)) |
                                        | L(n,e,v,b) =
P(p,l,a,n,(f x))   = (a f x)            |     x := <>
P(p,l,a,n,<x>)     = (p x)              |     f := (e x)
P(p,l,a,n,{n a b}) = (l n a b)          |     x <- R(n+1,f,v)
P(p,l,a,n,x:@}     = (n x)              |     R(n+1,f,b)
```



A Haskell implementation is about 180 lines, with examples:

```haskell
import Control.DeepSeq
import Numeric.Natural
import Data.List

data Val
    = PIN !Val
    | LAW !Natural !Natural !Val
    | APP !Val Val
    | NAT !Natural

instance NFData Val where
  rnf (APP f x) = rnf f `seq` rnf x
  rnf _         = ()

f % x | arity f == 1 = subst f [x]
f % x | otherwise    = APP f x

arity (APP f _)     = arity f - 1
arity (PIN (NAT n)) = case n of 1->3; 3->3; 4->5; _->1
arity (PIN i)       = arity i
arity (LAW _ a _)   = fromIntegral a :: Integer
arity (NAT n)       = 0

pat p _ _ _ (PIN x)     = (p % x)
pat _ l _ _ (LAW n a b) = (l % NAT n % NAT a % b)
pat _ _ a _ (APP f x)   = (a % f % x)
pat _ _ _ n x@NAT{}     = (n % x)

kal n e (NAT b) | b<=n          = e !! fromIntegral (n-b)
kal n e (NAT 0 `APP` f `APP` x) = (kal n e f % kal n e x)
kal _ _ (NAT 2 `APP` x)         = x
kal _ _ x                       = x

run :: Natural -> [Val] -> Val -> Val
run arity ie body = res
  where (n, e, res::Val) = go arity ie body
        go i acc (NAT 1 `APP` v `APP` k') = go (i+1) (kal n e v : acc) k'
        go i acc x                        = (i, acc, kal n e x)

subst (APP f x)       xs = subst f (x:xs)
subst x@(PIN l@LAW{}) xs = exec l (x:xs)
subst (PIN i)         xs = subst i xs
subst x               xs = exec x (x:xs)

nat (NAT n) = n
nat _       = 0

exec (LAW n a b) xs                    = run a (reverse xs) b
exec (NAT 0)     [_,x]                 = PIN (force x)
exec (NAT 1)     [_,n,a,b] | nat a > 0 = LAW (nat n) (nat a) (force b)
exec (NAT 2)     [_,x]                 = NAT (nat x + 1)
exec (NAT 3)     [_,z,p,x]             = case nat x of 0->z; n->(p % NAT (n-1))
exec (NAT 4)     [_,p,l,a,n,x]         = pat p l a n x
exec f           e                     = error $ show ("crash", (f:e))


-- Some Examples ---------------------------------------------------------------

instance Num Val where fromInteger i = NAT (fromIntegral i)

showApp (APP f x) xs = showApp f (x:xs)
showApp f         xs = "(" <> intercalate " " (show <$> (f:xs)) <> ")"

instance Show Val where
    show (PIN i)     = concat ["<", show i, ">"]
    show (LAW n a b) = concat ["{", show n, " ", show a, " ", show b, "}"]
    show (APP f x)   = showApp f [x]
    show (NAT n)     = show n

pin=(PIN 0)
law=(PIN 1)
inc=(PIN 2)
natCase=(PIN 3)
planCase=(PIN 4)

k  = law % 0 % 2 % 1        --  k a _ = a
k3 = law % 0 % 4 % 1        --  k3 v _ _ _ = v
i  = law % 0 % 1 % 1        --  i x = x
z1 = law % 0 % 1 % (2 % 0)  --  z1 _ = 0
z3 = law % 0 % 3 % (2 % 0)  --  z3 _ _ _ = 0

appHead=(planCase % 0 % 0 % k % 0)
toNat=(natCase % 0 % inc)
dec=(natCase % 0 % i)

-- helpers for building laws
a f x = (0 % f % x)
lawE n a b = (law % n % a % b)

-- length of an array
--
--     lenHelp len h t = inc (len h)
--     len x = planCase z1 z3 (\len h t -> inc (len h))  n x
lenHelp = lawE 0 3 (inc `a` (1 `a` 2))
len = lawE 0 1 (((planCase % z1 % z3) `a` (lenHelp `a` 0)) `a` z1 `a` 1)

-- getting the tag of an ADT:
--
-- head (APP h t) = h
-- head x         = x
--
-- tag x = toNat (head x)
head' = lawE 0 3 (1 `a` 2)    -- \head h t -> head h
headF = lawE 0 1 (planCase `a` (k `a` 1) `a` (k3 `a` 1) `a` (head' `a` 0) `a` i `a` 1)
tag   = lawE 0 1 (toNat `a` (headF `a` 1))

chk :: Val -> Val -> IO ()
chk x y = do
    putStrLn ("assert " <> show x <> " " <> show y)
    if (x == y) then pure () else error "FAIL"

deriving instance Eq Val

main = do
    -- increment, make a law, make a pin
    chk 5           $ inc % 4
    chk (LAW 1 2 3) $ law % 1 % 2 % 3
    chk (PIN 5)     $ pin % (inc % 4)

    -- pattern match on nats
    chk 9 $ toNat % 9

    chk 0 $ toNat % (pin % 9)

    -- pattern match on PLAN values
    chk (1%2)     (planCase % 1 % 0 % 0 % 0 % (pin%2))
    chk (1%2%3%4) (planCase % 0 % 1 % 0 % 0 % (law%2%3%4))
    chk (1%2%3)   (planCase % 0 % 0 % 1 % 0 % (2%3))
    chk (1%2)     (planCase % 0 % 0 % 0 % 1 % 2)

    -- basic laws
    chk (LAW 0 2 0) $ law % 0 % 2 % 0 % 7 % 8
    chk 7           $ law % 0 % 2 % 1 % 7 % 8
    chk 8           $ law % 0 % 2 % 2 % 7 % 8
    chk 3           $ law % 0 % 2 % 3 % 7 % 8

    -- force a value by using it to build a law and the running it.
    chk 1 (law % 0 % 1 % (2 % 1) % 0)

    -- select finite part of infinite value
    chk 1 (appHead % (law % 99 % 1 % (1 % (0%1%2) % 2) % 1))

    -- running pins:
    chk (LAW 1 2 0)      $ pin % law % 1 % 2 % 0
    chk (LAW 1 2 0)      $ pin % (law%1) % 2 % 0
    chk (PIN(LAW 1 2 0)) $ pin % (law%1%2%0) % 3 % 4
    chk (PIN(LAW 1 2 0)) $ pin % (pin % (law%1%2%0)) % 3 % 4

    chk 9 ( law % 0 % 1 % 1 % 9 )
    chk 8 ( law % 0 % 1 % (1 % 1 % 2) % 8 )

    chk 7 ( law % 0 % 1 %  --  ? ($0 $1)
               (1 % 3 %    --  @ $2 = $3
               (1 % 7 %    --  @ $3 = 9
               2))         --  $2
           % 9)

    -- more complex example
    chk (1%(0%2))
             (law%0%1%           --   | ? ($0 $1)
               (1% (0%(2%0)%3)%  --     @ $2 = (0 $3)
               (1% (2%2)%        --     @ $3 = 2
                (0%1%2)))%       --     | ($1 $2)
             1)                  --   1

    -- trivial cycles are okay if not used.
    chk 7 ( (law % 0 % 1 %  --   | ? ($0 $1)
              (1% 7%        --     @ $2 = 7
              (1% 3%        --     @ $3 = $3
                            --     $2
               2))%         --   9
            9))

    -- length of array
    chk 9 (len % (0 % 1 % 2 % 3 % 4 % 5 % 6 % 7 % 8 % 9))

    -- head of closure
    chk 7   (headF % (7 % 1 % 2 % 3 % 4 % 5 % 6 % 7 % 8 % 9))
    chk law (headF % (law % 1 % 2))

    -- tag of ADT (head cast to nat)
    chk 7 (tag % (7 % 1 % 2 % 3 % 4 % 5 % 6 % 7 % 8 % 9))
    chk 0 (tag % (law % 1 % 2))
```
