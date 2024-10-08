-- An alternate version of PLAN.hs that demonstrates jets and arrays.

import Control.DeepSeq
import Data.List
import Numeric.Natural
import Prelude

-- Imagine this is an array.  This uses a list just to avoid a dependency.
type Array a = [a]

data Val
    = PIN { i :: !Val, h :: !Natural, e :: !(Maybe ([Val] -> Val)) }
    | LAW !Natural !Natural !Val
    | APP !Val Val
    | NAT !Natural
    | ROW !Natural !(Array Val) -- 1[3 4 5] == (1 3 4 5)

instance NFData Val where
  rnf (APP f x) = rnf f `seq` rnf x
  rnf _         = ()

f % x | arity f == 1 = subst f [x]
f % x | otherwise    = APP f x

arity (APP f _)    = arity f - 1
arity PIN{i=NAT 0} = 1
arity PIN{i=NAT 1} = 3
arity PIN{i=NAT 2} = 1
arity PIN{i=NAT 3} = 6
arity PIN{i=NAT _} = 1
arity (PIN i _ _)  = arity i
arity (LAW _ a _)  = fromIntegral a :: Integer
arity (NAT{})      = 1
arity (ROW{})      = 1

cas p _ _ _ _ PIN{i}      = (p % i)
cas _ l _ _ _ (LAW n a b) = (l % NAT n % NAT a % b)
cas _ _ a _ _ (APP f x)   = (a % f % x)
cas _ _ _ z _ (NAT 0)     = z
cas _ _ _ _ m x@(NAT n)   = (m % NAT (n-1))
cas _ _ a _ _ (ROW n [x]) = (a % NAT n % x)
cas _ _ a _ _ (ROW n xs)  = (a % ROW n (init xs) % last xs)

-- TODO: Update this with letrec.
run n e (NAT b) | b<=n = e !! fromIntegral (n-b)
run n e (ROW 0 [f,x])  = (run n e f % run n e x)
run _ _ (ROW 0 [x])    = x
run n e (ROW 1 [v,b])  = run (n+1) f b where f = (run (n+1) f v : e)
run _ _ x                         = x

subst (APP f x)            xs = subst f (x:xs)
subst x@(PIN _ _ (Just e)) xs = e (x:xs)
subst x@(PIN l@LAW{} _ _)  xs = exec l (x:xs)
subst x@(PIN NAT{} _ _)    xs = exec x (x:xs)
subst (PIN i _ _)          xs = subst i xs
subst x                    xs = exec x (x:xs)

nat (NAT n) = n
nat _       = 0

pinJet :: Natural -> Maybe ([Val] -> Val)
pinJet 128221131686620819151974758560989378 = Just (\[_,x,y] -> NAT (nat x + nat y))
pinJet _haz                                 = Nothing

-- Fake implementation to avoid needing dependencies.  This needs to be
-- a cryptographic hash.  See Fan.Hash for a real version of this.
fanHash :: Natural -> Val -> Natural
fanHash !acc (NAT n)     = ((acc*64)+0) + n
fanHash !acc (PIN _ h _) = ((acc*64)+1) + h
fanHash !acc (APP f x)   = fanHash (((acc*64)+2) + fanHash 0 f) x
fanHash !acc (LAW n a b) = fanHash (((acc*64)+3) + n + a) b
fanHash !acc (ROW n [])  = fanHash acc $ NAT n
fanHash !acc (ROW n xs)  = fanHash acc $ APP (ROW n $ init xs) (last xs)

pin x = x `deepseq` PIN x haz (pinJet haz) where haz = fanHash 0 x

exec :: Val -> [Val] -> Val
exec (LAW _ a b)  xs                    = run a (reverse xs) b
exec (ROW n xs)   [_,x]                 = ROW n (xs <> [x])
exec (NAT n)      [_,x]                 = ROW n [x]
exec PIN{i=NAT 0} [_,x]                 = pin x
exec PIN{i=NAT 1} [_,n,a,b] | nat a > 0 = LAW (nat n) (nat a) (force b)
exec PIN{i=NAT 2} [_,x]                 = NAT (nat x + 1)
exec PIN{i=NAT 3} [_,p,l,a,z,m,o]       = cas p l a z m o
exec f            e                     = error $ show ("crash", f, e)


-- Some Examples ---------------------------------------------------------------

instance Num Val where
    fromInteger i = NAT (fromIntegral i)

instance Show Val where
    show (NAT n)     = show n
    show (PIN i _ _) = concat ["<", show i, ">"]
    show (LAW n a b) = concat ["{", show n, " ", show a, " ", show b, "}"]
    show (ROW 0 xs)  = show xs
    show (ROW n xs)  = show n <> show xs
    show (APP f x)   = goApp f [x]
      where goApp (APP f x) xs = goApp f (x:xs)
            goApp f         xs = "(" <> intercalate " " (show <$> (f:xs)) <> ")"

_Pin=(pin 0)
_Law=(pin 1)
_Inc=(pin 2)
_Case=(pin 3)

lawE n a b = (_Law % n % a % b)

cnst=(_Law % 0 % 2 % 1)
cnst2=(_Law % 0 % 3 % 1)
cnst3=(_Law % 0 % 4 % 1)
ignore=(_Law % 0 % 2 % 2)
identity=(_Law % 0 % 1 % 1)

-- (PlanCase p l a n x)=(Case p l a n _&n x)
planCase = lawE 0 5 (_Case `a` 1 `a` 2 `a` 3 `a` 4 `a` (cnst `a` 4) `a` 5)


-- (NatCase z p x)=(Case _&z (_ _ _)&z (_ _)&z z p x)
natCase = lawE 0 3 $ _Case `a` (cnst  `a` 1)
                           `a` (cnst3 `a` 1)
                           `a` (cnst2 `a` 1)
                           `a` 1
                           `a` 2
                           `a` 3

appHead=(planCase % 0 % 0 % cnst % 0)
appTail=(planCase % 0 % 0 % ignore % 0)
toNat=(natCase % 0 % 3)
dec=(natCase % 0 % identity)


-- (_Times a b c)=(2 b (_Times a a-b) c)
_Times=(_Pin %
       (_Law % 126879464510559 % 3 % -- lawName="_Times"
        (0 % (0 % (0 % (0 % natCase) % 2)
                     % (0 % (0 % 0 % 1)
                          % (0 % 1 % 2)))
           % 3)))

-- (_ToNat a)=(2:0:3 a)
_ToNat=(_Pin % (_Law % 127961276568671 % 1 % (0 % (natCase % 0 % _Inc) % 1)))

addLaw= ( _Law % 1684291935 % 2 %
          (0 % (0 % (_Times % _Inc) % (0 % _ToNat % 1)) % 2))

-- (_Add a b)=(_Times:_Inc _ToNat-a b)
_Add=(_Pin % addLaw)

a f x = 0 % f % x

-- mapApp map f h t = map f h (f t)
doMapApp = ( _Law % 0 % 4 % (1 `a` 2 `a` 3 `a` (2 `a` 4)) )

-- map f (APP h t) = (map f h (f t))
-- map f h         = h
doMap = ( _Law % 0 % 2 %
          ( planCase `a` (cnst `a` 2)
                      `a` (cnst3 `a` 2)
                      `a` (doMapApp `a` 0 `a` 1)
                      `a` (cnst `a` 2)
                      `a` 2 ))

main = do

    print (_ToNat % 3)
    print (_ToNat % _ToNat)
    print (_Times % _Inc % 3 % 4)
    print (_Add % 3 % 4)
    print (_Law % 0 % 2 % (0 % 1 % 2) % 8 % 9)
    -- print (pin 99 % 88)

    -- arrays
    print (0 % 0 % 1 % 2)
    print (1 % 5)

    print ( planCase % 777)
    print (appHead % (1 % 2))
    print (appHead % (1 % 2 % 3))
    print (appTail % (1 % 2 % 3))

    -- increment, make a law, make a pin
    print (_Inc % 4)
    print (_Law % 1 % 2 % 3)
    print (_Pin % (3 % 4))

    -- pattern match on nats
    print (toNat % 9)
    print (toNat % (4 % 9))

    -- pattern match on PLAN values
    print (planCase % 1 % 0 % 0 % 0 % (_Pin % 2))
    print (planCase % 0 % 1 % 0 % 0 % (_Law % 2 % 3 % 4))
    print (planCase % 0 % 0 % 1 % 0 % (2 % 3))
    print (planCase % 0 % 0 % 0 % 1 % 2)

    -- force a value by using it to build a law and the running it.
    print (_Law % 0 % 1 % (2 % 1) % 0)

    -- select finite part of infinite value
    print (appHead % (_Law % 99 % 1 % (1 % (0 % 1 % 2) % 2) % 1))

    -- running pins:
    print $ (_Pin % _Law) % 1 % 2 % 0
    print $ (_Pin % (_Law % 1)) % 2 % 0
    print $ (_Pin % (_Law % 1 % 2 % 0)) % 3 % 4
    print $ (_Pin % (_Pin % (_Law % 1 % 2 % 0))) % 3 % 4

    print (fanHash 0 addLaw)

    -- executing a jet
    print ( _Add % 999999999999 % 1000000000001 )

    -- mapping (add 9999) over an array
    print ( doMap % (_Add % 9999) % (0 % 0 % 1 % 2 % 4 % 5))

    putStrLn ("OKAY")
