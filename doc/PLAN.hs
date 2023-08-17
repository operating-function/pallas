{-# LANGUAGE DeriveAnyClass, LambdaCase #-}

import Control.DeepSeq
import GHC.Generics
import Numeric.Natural
import Data.List

data Val
    = PIN !Val
    | LAW !Natural !Natural !Val
    | APP Val Val
    | NAT !Natural
 deriving (Generic, NFData)

nat (NAT n) = n
nat _       = 0

f % x = if arity f == 1 then exec f [x] else APP f x

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

exec (PIN i)     xs          = exec i xs
exec (LAW n a b) xs          = run a (reverse (LAW n a b : xs)) b
exec (APP f x)   xs          = exec f (x:xs)
exec (NAT 0)     [n,NAT 0,b] = r where r = (nat n,b) `deepseq` run 0 [r] b
exec (NAT 0)     [n,a,b]     = LAW (nat n) (nat a) (force b)
exec (NAT 1)     [p,l,a,n,x] = pat p l a n x
exec (NAT 2)     [z,p,x]     = case nat x of 0 -> z; n -> p % NAT (n-1)
exec (NAT 3)     [x]         = NAT (nat x + 1)
exec (NAT 4)     [x]         = PIN (force x)
exec f           e           = error $ show ("crash", (f:e))


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
appHead=(1 % 0 % 0 % cnst % 0)
toNat=(2 % 0 % 3)
mkPin=4
mkLaw=0
inc=3

main = do
    -- increment, make pins and laws
    print (inc % 4)
    print (mkLaw % 1 % 2 % 3)
    print (mkPin % (3 % 4))

    -- pattern match on nats
    print (toNat % 9)
    print (toNat % (4 % 9))

    -- zero-argument law
    print (0 % 0 % 0 % 1)

    -- select finite part of infinite value
    print (appHead % (0 % 0 % 0 % (0 % 1 % 0)))

    -- pattern match
    print (1 % 1 % 0 % 0 % 0 % (4%4))
    print (1 % 0 % 1 % 0 % 0 % (0%1%2%3))
    print (1 % 0 % 0 % 1 % 0 % (1%2))
    print (1 % 0 % 0 % 0 % 1 % 9)
