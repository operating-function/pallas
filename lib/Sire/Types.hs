-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE NoFieldSelectors #-}

{-|
    Types for Sire syntax trees (`Cmd`, `Exp`, etc).  "Sire.Syntax"
    parses `Rex` into concrete syntax trees, and "Sire.ReplExe"
    does... everything else (TODO: modularize)
-}
module Sire.Types
    ( SireState(..)
    , Binding(..)
    , BindingData(..)
    , mkNewBinding
    , ToBind(..)
    , Lam(..)
    , Sire(..)
    , rexText
    , planRex
    , planText
    , sireRex
    , lamRex
    , trk
    , trkM
    , apple
    , apple_
    , traceSire
    , traceSire'
    , traceSireId
    )
where

import PlunderPrelude

import Data.Sorted  (Tab)
import Fan          (Fan(..), mkPin)
import Fan.Convert  (ToNoun(toNoun))
import Fan.JetImpl  (doTrk)
import Loot.Backend (loadShallow)
import Loot.ReplExe (showValue)
import Loot.Syntax  (joinRex)
import Rex          (GRex(..), RexColorScheme(NoColors), RuneShape(..),
                     TextShape(..), rexFile)

-- Aliases ---------------------------------------------------------------------

type Rex  = GRex Any


-- Types -----------------------------------------------------------------------

data ToBind = TO_BIND (Maybe Nat) (Maybe Sire) Nat Sire


-- Formal Sire State -----------------------------------------------------------

type Str = Nat
type Any = Fan

type Props = Tab Any (Tab Any Any)

type Scope = Tab Any Binding

data SireState = SIRE_STATE
    { nextKey :: Nat                     --  Next unique key.
    , context :: Str                     --  The name of the current module
    , scope   :: Scope                   --  Current global namespace.
    , modules :: Tab Any (Scope, Props)  --  Loaded modules.
    , allProps:: Props                   --  All bindings by key.
    }

{-
    When bindings are decoded from PLAN, we *remember* the original
    PLAN value.  This way, if we convert back to a noun, we can just
    use the old value.  This avoids the needs to reconstruct the noun,
    and preserves sharing (only one version of the binding need exist
    in memory).

    This is especially important, because binding noun are *huge*.
    A binding inclides source code, and that source code inclues other
    bindings, etc.

    Sire-in-sire wont need to deal with this, since there is no separation
    between the binding and the underlying noun.
-}
data Binding = BINDING
    { d    :: BindingData
    , noun :: Any
    }
  deriving (Eq, Ord, Show)

data BindingData = BINDING_DATA
    { key      :: Nat          --  The binding-key of the binder.
    , value    :: Any          --  The value of the binder
    , code     :: Sire         --  Source for inlining (unoptimized, unlifted)
    , location :: Any          --  What module was this defined in?
    , name     :: Any          --  What name was this defined under?
    }
  deriving (Eq, Ord, Show)

mkNewBinding :: BindingData -> Binding
mkNewBinding d =
    BINDING d noun
  where
    list = [NAT d.key, d.value, sireNoun d.code, d.location, d.name]
    noun = mkPin $ ROW $ arrayFromListN 5 list


-- Sire Types ------------------------------------------------------------------

-- This lazily loads a state object and crashes if something isn't
-- as expected.  This is intended only for doing queries on specific
-- components of the state, doing a full load in this way is very
-- expensive.

data Lam = LAM
    { pin    :: Bool
    , inline :: Bool
    , tag    :: Nat
    , args   :: Nat
    , body   :: Sire
    }
  deriving (Eq, Ord, Show)

-- This is the internal representation that is used for inlining.
-- All names have been resolved, globals point directly to their bindings.
data Sire
    = S_VAR Nat
    | S_VAL Any
    | S_GLO Binding
    | S_APP Sire Sire
    | S_LET Sire Sire
    | S_LIN Sire
    | S_LAM Lam
  deriving (Eq, Ord, Show)

sireNoun :: Sire -> Any
sireNoun = go
  where
    goLam :: Lam -> [Any]
    goLam l = [ toNoun l.pin
              , toNoun l.inline
              , toNoun l.tag
              , toNoun l.args
              , go l.body
              ]

    go :: Sire -> Any
    go = \case
        S_VAR n   -> NAT n
        S_VAL n   -> ROW $ arrayFromListN 2 ["val", n]
        S_GLO b   -> ROW $ arrayFromListN 2 ["ref", b.noun]
        S_APP f x -> ROW $ arrayFromListN 3 ["app", go f, go x]
        S_LET v b -> ROW $ arrayFromListN 3 ["let", go v, go b]
        S_LIN x   -> ROW $ arrayFromListN 2 ["lin", go x]
        S_LAM l   -> ROW $ arrayFromListN 6 ("lam" : goLam l)

lamRex :: Lam -> Rex
lamRex l =
    N OPEN rune [hed] (Just $ openApp $ sireRex l.body)
  where
    hed = N NEST "|" hedSons Nothing

    hedSons =
        [ inlineMark (word $ showName $ NAT l.tag)
        , N PREF ".." [word (tshow l.args)] Nothing
        ]

    rune = if l.pin then "??" else "?"

    inlineMark rex | l.inline  = N PREF "**" [rex] Nothing
    inlineMark rex | otherwise = rex

word :: Text -> GRex a
word n = T WORD n Nothing

showName :: Any -> Text
showName = \case
    NAT n ->
        case natUtf8 n of
            Left{}   -> tshow n
            Right nm -> nm -- TODO What if it is valid text but not
                           -- valid WORD?  Handle that too.
    wut ->
        error $
           (<>) "bad state: binding.name is not a NAT"
                (unpack $ rexText $ C wut)


sireRex :: Sire -> Rex
sireRex = \case
    S_VAR v   -> N PREF "$" [word (tshow v)] Nothing
    S_VAL v   -> C v
    S_GLO b   -> gloRex b
    S_APP f x -> appRex f [x]
    S_LET v x -> N OPEN "@" [sireRex v] (Just $ openApp $ sireRex x)
    S_LIN sir -> N PREF "**" [sireRex sir] Nothing
    S_LAM lam -> lamRex lam
  where
    gloRex b =
        T WORD (showName b.d.name)
            $ Just
            $ N NEST "," [word (tshow b.d.key)]
            $ Nothing

    appRex (S_APP f x) xs = appRex f (x:xs)
    appRex f xs = niceApp (sireRex <$> (f:xs))

    niceApp xs = case (all isSimpleClosed xs, reverse xs) of
        ( _,     []   ) -> error "niceApp: impossible"
        ( True,  _    ) -> N NEST "|" xs           Nothing
        ( False, l:ls ) -> N OPEN "|" (reverse ls) (Just $ openApp l)

planRex :: Any -> GRex Void
planRex = showValue . loadShallow

planText :: Any -> Text
planText =
    let ?rexColors = NoColors
    in rexFile . planRex

rexText :: Rex -> Text
rexText =
    let ?rexColors = NoColors
    in rexFile . joinRex . fmap handleEmbed
  where
    handleEmbed :: Any -> GRex Void
    handleEmbed x = N style "↓" [rex] Nothing
      where rex   = planRex x
            style = if isClosed rex then PREF else OPEN

isClosed :: GRex a -> Bool
isClosed (N OPEN _ _ _) = False
isClosed _              = True

isSimpleClosed :: GRex a -> Bool
isSimpleClosed (N OPEN _ _ _)   = False
isSimpleClosed (N NEST "|" _ _) = False
isSimpleClosed _                = True

openApp :: GRex a -> GRex a
openApp (N NEST "|" ss h) = N OPEN "|" ss h
openApp rex               = rex


--------------------------------------------------------------------------------

trkM :: Monad m => Any -> m ()
trkM msg = do
    let !() = doTrk msg ()
    pure ()

trk :: Any -> a -> a
trk = doTrk

--------------------------------------------------------------------------------

apple :: Sire -> [Sire] -> Sire
apple = foldl' S_APP

apple_ :: [Sire] -> Sire
apple_ []     = error "apple_ given nothing to work with"
apple_ (f:xs) = apple f xs

traceSire' :: Text -> Sire -> a -> a
traceSire' context sire result =
    trk (REX it) result
  where
    it = N OPEN "#" [T WORD context Nothing]
       $ Just
       $ sireRex sire

traceSire :: Text -> Sire -> a -> a
traceSire _context _sire result = result
-- traceSire = traceSire'

traceSireId :: Text -> Sire -> Sire
traceSireId context sire = traceSire context sire sire
