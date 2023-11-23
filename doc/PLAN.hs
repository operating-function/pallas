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

arity (APP f _)   = arity f - 1
arity (PIN i)     = arity i
arity (LAW _ a _) = fromIntegral a :: Integer
arity (NAT n)     = case n of 0->3; 1->5; 2->3; _->1

pat p _ _ _ (PIN x)     = (p % x)
pat _ l _ _ (LAW n a b) = (l % NAT n % NAT a % b)
pat _ _ a _ (APP f x)   = (a % f % x)
pat _ _ _ n x@NAT{}     = (n % x)

run n e (NAT b) | b<=n          = e !! fromIntegral (n-b)
run n e (NAT 0 `APP` f `APP` x) = (run n e f % run n e x)
run n e (NAT 1 `APP` v `APP` b) = run (n+1) f b where f = (run (n+1) f v : e)
run _ _ (NAT 2 `APP` x)         = x
run _ _ x                       = x

subst (APP f x)       xs = subst f (x:xs)
subst x@(PIN l@LAW{}) xs = exec l (x:xs)
subst (PIN i)         xs = subst i xs
subst x               xs = exec x (x:xs)

nat (NAT n) = n
nat _       = 0

exec (LAW n a b) xs                   = run a (reverse xs) b
exec (NAT 0)     [_,n,a,b] | nat(a)>0 = LAW (nat n) (nat a) (force b)
exec (NAT 1)     [_,p,l,a,n,x]        = pat p l a n x
exec (NAT 2)     [_,z,p,x]            = case nat x of 0 -> z; n -> p % NAT (n-1)
exec (NAT 3)     [_,x]                = NAT (nat x + 1)
exec (NAT 4)     [_,x]                = PIN (force x)
exec f           e                    = error $ show ("crash", (f:e))


-- Some Examples ---------------------------------------------------------------

instance Num Val where fromInteger i = NAT (fromIntegral i)

showApp (APP f x) xs = showApp f (x:xs)
showApp f         xs = "(" <> intercalate " " (show <$> (f:xs)) <> ")"

instance Show Val where
    show (PIN i)     = concat ["<", show i, ">"]
    show (LAW n a b) = concat ["{", show n, " ", show a, " ", show b, "}"]
    show (APP f x)   = showApp f [x]
    show (NAT n)     = show n

cnst=(0 % 0 % 2 % 1)
identity=(0 % 0 % 1 % 1)
appHead=(1 % 0 % 0 % cnst % 0)
toNat=(2 % 0 % 3)
mkPin=4
mkLaw=0
inc=3
dec=(2 % 0 % identity)

main = do
    -- increment, make a law, make a pin
    print (inc % 4)
    print (mkLaw % 1 % 2 % 3)
    print (mkPin % (3 % 4))

    -- pattern match on nats
    print (toNat % 9)
    print (toNat % (4 % 9))

    -- pattern match on PLAN values
    print (1 % 1 % 0 % 0 % 0 % (4%2))
    print (1 % 0 % 1 % 0 % 0 % (0%2%3%4))
    print (1 % 0 % 0 % 1 % 0 % (2%3))
    print (1 % 0 % 0 % 0 % 1 % 2)

    -- select finite part of infinite value
    print (appHead % (0 % 0 % 2 % (1 % (0 % 0 % 2) % 2) % 7 % 6))

    -- running pins:
    print ( (4%0) % 1 % 2 % 0 )
    print ( (4%(0%1)) % 2 % 0 )
    print ( (4%(0%1%2%0)) % 3 % 4)
    print ( (4%(4%(0%1%2%0))) % 3 % 4)
