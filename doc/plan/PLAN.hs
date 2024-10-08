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
arity (PIN (NAT n)) = case n of 1->3; 3->6; _->1
arity (PIN i)       = arity i
arity (LAW _ a _)   = fromIntegral a :: Integer
arity (NAT n)       = 0

cas p _ _ _ _ (PIN x)     = (p % x)
cas _ l _ _ _ (LAW n a b) = (l % NAT n % NAT a % b)
cas _ _ a _ _ (APP f x)   = (a % f % x)
cas _ _ _ z _ (NAT 0)     = z
cas _ _ _ _ m (NAT n)     = (m % NAT (n-1))

kal n e (NAT b) | b<=n          = e !! fromIntegral (n-b)
kal n e (NAT 0 `APP` f `APP` x) = (kal n e f % kal n e x)
kal _ _ (NAT 0 `APP` x)         = x
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
exec (NAT 3)     [_,p,l,a,z,m,o]       = cas p l a z m o
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

_Pin=(PIN 0)
_Law=(PIN 1)
_Inc=(PIN 2)
_Case=(PIN 3)

k  = _Law % 0 % 2 % 1        --  k a _ = a
k2 = _Law % 0 % 3 % 1        --  k2 a _ _ = a
k3 = _Law % 0 % 4 % 1        --  k3 v _ _ _ = a
i  = _Law % 0 % 1 % 1        --  i x = x
z1 = _Law % 0 % 1 % (0 % 0)  --  z1 _ = 0
z3 = _Law % 0 % 3 % (0 % 0)  --  z3 _ _ _ = 0

a f x = (0 % f % x)
lawE n a b = (_Law % n % a % b)

-- (PlanCase p l a n x)=(Case p l a n _&n x)
planCase = lawE 0 5 (_Case `a` 1 `a` 2 `a` 3 `a` 4 `a` (k `a` 4) `a` 5)


-- (NatCase z p x)=(Case _&z (_ _ _)&z (_ _)&z z p x)
natCase = lawE 0 3 $ _Case `a` (k  `a` 1)
                           `a` (k3 `a` 1)
                           `a` (k2 `a` 1)
                           `a` 1
                           `a` 2
                           `a` 3

appHead=(_Case % 0 % 0 % k % 0 % 0)
toNat=(natCase % 0 % _Inc)
dec=(natCase % 0 % i)

-- helpers for building laws

-- length of an array
--
--     lenHelp len h t = _Inc (len h)
--     len x = planCase z1 z3 (\len h t -> inc (len h))  n x
lenHelp = lawE 0 3 (_Inc `a` (1 `a` 2))
len = lawE 0 1 (((planCase % z1 % z3) `a` (lenHelp `a` 0)) `a` 0 `a` 1)

-- getting the tag of an ADT:
--
-- head (APP h t) = h
-- head x         = x
--
-- tag x = toNat (head x)
head' = lawE 0 3 (1 `a` 2)    -- \head h t -> head h
headF = lawE 0 1 (planCase `a` (k `a` 1) `a` (k3 `a` 1) `a` (head' `a` 0) `a` 1 `a` 1)
tag   = lawE 0 1 (toNat `a` (headF `a` 1))

chk :: Val -> Val -> IO ()
chk x y = do
    putStrLn ("assert " <> show x <> " " <> show y)
    if (x == y) then pure () else error "FAIL"

deriving instance Eq Val

main = do
    -- increment, make a _Law, make a pin
    chk 5           $ _Inc % 4
    chk (LAW 1 2 3) $ _Law % 1 % 2 % 3
    chk (PIN 5)     $ _Pin % (_Inc % 4)

    -- pattern match on nats
    chk 9 $ toNat % 9

    chk 0 $ toNat % (_Pin % 9)

    -- pattern match on PLAN values
    chk (1%2)     (planCase % 1 % 0 % 0 % 0 % (_Pin%2))
    chk (1%2%3%4) (planCase % 0 % 1 % 0 % 0 % (_Law%2%3%4))
    chk (1%2%3)   (planCase % 0 % 0 % 1 % 0 % (2%3))
    chk 1         (planCase % 0 % 0 % 0 % 1 % 2)

    -- basic laws
    chk (LAW 0 2 0) $ _Law % 0 % 2 % 0 % 7 % 8
    chk 7           $ _Law % 0 % 2 % 1 % 7 % 8
    chk 8           $ _Law % 0 % 2 % 2 % 7 % 8
    chk 3           $ _Law % 0 % 2 % 3 % 7 % 8

    -- force a value by using it to build a _Law and the running it.
    chk 1 (_Law % 0 % 1 % (0 % 1) % 0)

    -- select finite part of infinite value
    chk 1 (appHead % (_Law % 99 % 1 % (1 % (0%1%2) % 2) % 1))

    -- running pins:
    chk (LAW 1 2 0)      $ _Pin % _Law % 1 % 2 % 0
    chk (LAW 1 2 0)      $ _Pin % (_Law%1) % 2 % 0
    chk (PIN(LAW 1 2 0)) $ _Pin % (_Law%1%2%0) % 3 % 4
    chk (PIN(LAW 1 2 0)) $ _Pin % (_Pin % (_Law%1%2%0)) % 3 % 4

    chk 9 ( _Law % 0 % 1 % 1 % 9 )
    chk 8 ( _Law % 0 % 1 % (1 % 1 % 2) % 8 )

    chk 7 ( _Law % 0 % 1 %  --  ? ($0 $1)
               (1 % 3 %    --  @ $2 = $3
               (1 % 7 %    --  @ $3 = 9
               2))         --  $2
           % 9)

    -- more complex example
    chk (1%(0%2))
             (_Law%0%1%           --   | ? ($0 $1)
               (1% (0%(0%0)%3)%  --     @ $2 = (0 $3)
               (1% (0%2)%        --     @ $3 = 2
                (0%1%2)))%       --     | ($1 $2)
             1)                  --   1

    -- trivial cycles are okay if not used.
    chk 7 ( (_Law % 0 % 1 %  --   | ? ($0 $1)
              (1% 7%        --     @ $2 = 7
              (1% 3%        --     @ $3 = $3
                            --     $2
               2))%         --   9
            9))

    -- length of array
    chk 9 (len % (0 % 1 % 2 % 3 % 4 % 5 % 6 % 7 % 8 % 9))

    -- head of closure
    chk 7   (headF % (7 % 1 % 2 % 3 % 4 % 5 % 6 % 7 % 8 % 9))
    chk _Law (headF % (_Law % 1 % 2))

    -- tag of ADT (head cast to nat)
    chk 7 (tag % (7 % 1 % 2 % 3 % 4 % 5 % 6 % 7 % 8 % 9))
    chk 0 (tag % (_Law % 1 % 2))
