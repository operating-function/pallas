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

kal n e (NAT b) | b<=n          = e !! fromIntegral (n-b)
kal n e (NAT 0 `APP` f `APP` x) = (kal n e f % kal n e x)
kal _ _ (NAT 2 `APP` x)         = x
kal _ _ x                       = x

run :: Natural -> [Val] -> Val -> Val
run arity ie body = res
  where (n, e, res::Val) = go arity ie body
        go i acc (NAT 1 `APP` v `APP` k) = go (i+1) (kal n e v : acc) k
        go i acc x                       = (i, acc, kal n e x)

subst (APP f x)       xs = subst f (x:xs)
subst x@(PIN l@LAW{}) xs = exec l (x:xs)
subst (PIN i)         xs = subst i xs
subst x               xs = exec x (x:xs)

nat (NAT n) = n
nat _       = 0

exec (LAW n a b) xs                    = run a (reverse xs) b
exec (NAT 0)     [_,n,a,b] | nat a > 0 = LAW (nat n) (nat a) (force b)
exec (NAT 1)     [_,p,l,a,n,x]         = pat p l a n x
exec (NAT 2)     [_,z,p,x]             = case nat x of 0->z; n->(p % NAT (n-1))
exec (NAT 3)     [_,x]                 = NAT (nat x + 1)
exec (NAT 4)     [_,x]                 = PIN (force x)
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

cnst=(0 % 0 % 2 % 1)
identity=(0 % 0 % 1 % 1)
appHead=(1 % 0 % 0 % cnst % 0)
toNat=(2 % 0 % 3)
mkPin=4
mkLaw=0
inc=3
dec=(2 % 0 % identity)

chk :: Val -> Val -> IO ()
chk x y = do
    putStrLn ("assert " <> show x <> " " <> show y)
    if (x == y) then pure () else error "FAIL"

deriving instance Eq Val

main = do
    -- increment, make a law, make a pin
    chk 5           $ inc % 4
    chk (LAW 1 2 3) $ mkLaw % 1 % 2 % 3
    chk (PIN 5)     $ mkPin % (3 % 4)

    -- pattern match on nats
    chk 9 $ toNat % 9
    chk 0 $ toNat % (4 % 9)

    -- pattern match on PLAN values
    chk (1%2)     (1 % 1 % 0 % 0 % 0 % (4%2))
    chk (1%2%3%4) (1 % 0 % 1 % 0 % 0 % (0%2%3%4))
    chk (1%2%3)   (1 % 0 % 0 % 1 % 0 % (2%3))
    chk (1%2)     (1 % 0 % 0 % 0 % 1 % 2)

    -- basic laws
    chk (LAW 0 2 0) $ 0 % 0 % 2 % 0 % 7 % 8
    chk 7           $ 0 % 0 % 2 % 1 % 7 % 8
    chk 8           $ 0 % 0 % 2 % 2 % 7 % 8
    chk 3           $ 0 % 0 % 2 % 3 % 7 % 8

    -- force a value by using it to build a law and the running it.
    chk 1 (0 % 0 % 1 % (2 % 1) % 0)

    -- select finite part of infinite value
    chk 1 (appHead % (0 % 99 % 1 % (1 % (0%1%2) % 2) % 1))

    -- running pins:
    chk (LAW 1 2 0)      $ 4%0 % 1 % 2 % 0
    chk (LAW 1 2 0)      $ 4%(0%1) % 2 % 0
    chk (PIN(LAW 1 2 0)) $ 4%(0%1%2%0) % 3 % 4
    chk (PIN(LAW 1 2 0)) $ 4%(4%(0%1%2%0)) % 3 % 4

    chk 9 ( 0%0%1%1%9 )
    chk 8 ( 0 % 0 % 1 % (1 % 1 % 2) % 8 )

    chk 7 ( 0 % 0 % 1 % -- ? ($0 $1)
               (1 % 3 %  -- @ $2 = $3
               (1 % 7 %  -- @ $3 = 9
               2))       -- $2
           % 9)

    -- more complex example
    chk (1%(0%2))
             (0%0%1%            --   | ? ($0 $1)
               (1% (0%(2%0)%3)% --     @ $2 = (0 $3)
               (1% (2%2)%       --     @ $3 = 2
                (0%1%2)))%      --     | ($1 $2)
             1)                 --   1

    -- trivial cycles are okay if not used.
    chk 7 ( (0%0%1%  --   | ? ($0 $1)
              (1% 7% --     @ $2 = 7
              (1% 3% --     @ $3 = $3
                     --     $2
               2))%  --   9
            9))
