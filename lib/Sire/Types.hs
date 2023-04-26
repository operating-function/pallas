{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-|
    Types for Sire syntax trees (`Cmd`, `Exp`, etc).  "Sire.Syntax"
    parses `Rex` into concrete syntax trees, and "Sire.ReplExe"
    does... everything else (TODO: modularize)
-}
module Sire.Types
    ( Symb
    , Cmd(..)
    , TestEql(..)
    , Fun(..)
    , Exp(..)
    , XCmd
    , XExp
    , XFun
    , Fan
    , Bind(..)
    )
where

import PlunderPrelude

import Fan        (Fan, LawName)
import Loot.Types (Symb)
import Rex        (Rex)

---------------
-- Functions --
---------------

type XFun = Fun Symb Symb
type XExp = Exp Symb Symb
type XCmd = Cmd Symb Symb

{-|
    A Sire function has an identifier for self-reference, a `LawName`,
    a non-empty list of arguments, and an body expression.

    Note that when a function is bound (say with
    @(`ELET` v (`ELAM` (`FUN` w _ _ _)) _)@), there are two binders
    for the same function (@v@ and @w@).  @v@ is the binder used in the
    outside scope, and @w@ is use for self-references.  The difference
    doesn't matter during parsing, but it matters in code transformations.
-}
data Fun v a = FUN
    { inlinePls :: Bool
    , self      :: v
    , lawTag    :: LawName
    , args      :: [v]
    , body      :: Exp v a
    }
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

{-|
    Sire Expressions.  @v@ is the type of local variables, and @a@
    is the type of free variables.

    The parser just treats all references as free variables.  Later on,
    name resolution splits them apart.
-}
data Exp v a
    = EVAL Fan                    -- ^ An embedded plunder value
    | EREF a                      -- ^ A free variable.
    | EVAR v                      -- ^ A bound variable.
    | EAPP (Exp v a) (Exp v a)    -- ^ Function application
    | ELET v (Exp v a) (Exp v a)  -- ^ Let-binding
    | EREC v (Exp v a) (Exp v a)  -- ^ Self-recursive let binding.
    | ELAM Bool (Fun v a)         -- ^ Nested Function (Closure)
    | ELIN (NonEmpty (Exp v a))   -- ^ Explicit Inline Application
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

-------------------
-- REPL Commands --
-------------------

data TestEql  v a =
    TEST_EQL [Rex] (Exp v a) (Exp v a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

-- |Sire input commands.
data Cmd v a
    = CMDSEQ [Cmd v a]
        -- ^ @(* = x 3)(* = y 4)@ Multiple commands in block.

    | MODULE Text (Maybe Text)

    | IMPORT [(Text, Maybe (Set Symb))]
        -- ^ @(/+ foo [x y])@ Import @x@ and @y@ from `sire/foo.sire`

    | FILTER [Symb]
       -- ^ @(^-^ x y)@ Restrict the namespace to just x and y.

    | OUTPUT (Exp v a)
       -- ^ @(e)@ Eval+print @e@

    | DUMPIT (Exp v a)
       -- ^ @(<e)@ Eval+print @e@ and it's environment.

    | ASSERT [TestEql v a]
       -- ^ @!!= e f@ Assert that e==f

    | DEFINE [Bind v a]
       -- ^ @(x=y)@, @((f x)=x)@ Bind a value or function in the global
       --   namespace.

  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

-- |A binder.  It's either a function (takes arguments) or a value
-- (does not).
data Bind v a = BIND !Nat a (Exp v a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)
