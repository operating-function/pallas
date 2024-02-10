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
    , Bind(..)
    , BindData(..)
    , mkNewBind
    , ToBind(..)
    , Lam(..)
    , Sire(..)
    , rexText, pexText
    , planRex
    , planText
    , sireRex
    , lamRex
    , trk
    , trkM
    , trkRexM
    , apple
    , apple_
    , traceSire
    , traceSire'
    , traceSireId
    , _traceSireId
    )
where

import PlunderPrelude

import Sire.Backend
import Data.Sorted  (Tab)
import Fan          (Fan(..), mkPin)
import Fan.Convert  (ToNoun(toNoun))
import Fan.PlanRex  (PlanRex)
import Fan.JetImpl  (doTrk, doTrkRex)
import Loot.Backend (loadShallow)
import Loot.ReplExe (showValue, pexRender)
import Loot.Syntax  (joinRex)
import Rex          (GRex(..), RexColorScheme(NoColors), RuneShape(..),
                     TextShape(..), rexFile)


-- Aliases ---------------------------------------------------------------------

type Pex = PlanRex
type Rex = GRex Any


-- Types -----------------------------------------------------------------------

-- key=0 means "generate a key for me"
data ToBind = TO_BIND
    { key   :: Nat
    , props :: Maybe Sire
    , name  :: Str
    , value :: Sire
    }


-- Formal Sire State -----------------------------------------------------------

type Str = Nat
type Any = Fan

type Scope = Tab Any Bind

data SireState = SIRE_STATE
    { nextKey :: Nat            --  Next unique key.
    , context :: Str            --  The name of the current module
    , scope   :: Scope          --  Current global namespace.
    , modules :: Tab Any Scope  --  Loaded modules.
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
mkNewBind :: BindData -> Bind
mkNewBind d =
    BIND d $ mkPin $ toNoun
        (NAT d.key, d.value, sireNoun d.code, d.location, d.name, d.props)


-- Sire Types ------------------------------------------------------------------

sireNoun :: Sire -> Any
sireNoun = go
  where
    goLam :: Lam -> [Any]
    goLam l = [ toNoun l.pin
              , toNoun l.mark
              , toNoun l.recr
              , toNoun l.tag
              , toNoun l.args
              , go l.body
              ]

    goBinds :: [Sire] -> Any
    goBinds = ROW . fromList . map go

    go :: Sire -> Any
    go = \case
        V n   -> NAT n
        K n   -> ROW $ arrayFromListN 2 ["K", n]
        G b   -> ROW $ arrayFromListN 2 ["G", b.noun]
        A f x -> ROW $ arrayFromListN 3 ["A", go f, go x]
        L v b -> ROW $ arrayFromListN 3 ["L", go v, go b]
        R v b -> ROW $ arrayFromListN 3 ["R", goBinds v, go b]
        M x   -> ROW $ arrayFromListN 2 ["M", go x]
        F l   -> ROW $ arrayFromListN 7 ("F" : goLam l)

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

    inlineMark rex | l.mark  = N PREF "**" [rex] Nothing
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
    V v   -> N PREF "$" [word (tshow v)] Nothing
    K v   -> C v
    G b   -> gloRex b
    A f x -> appRex f [x]
    L v x -> N OPEN "@" [sireRex v] $ Just $ openApp $ sireRex x
    R v x -> case v of
                 []   -> sireRex x
                 a:as -> N OPEN "@@" [binds a as] $ Just $ openApp $ sireRex x
    M sir   -> N PREF "**" [sireRex sir] Nothing
    F lam   -> lamRex lam
  where
    binds v []     = N OPEN "=" [sireRex v] $ Nothing
    binds v (x:xs) = N OPEN "=" [sireRex v] $ Just (binds x xs)

    gloRex b =
        T WORD (showName b.bd.name)
            $ Just
            $ N NEST "," [word (tshow b.bd.key)]
            $ Nothing

    appRex (A f x) xs = appRex f (x:xs)
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

pexText :: Pex -> Text
pexText = rexText . pexRender (fmap absurd . planRex)

isClosed :: GRex a -> Bool
isClosed (N OPEN _ _ _) = False
isClosed (T LINE _ _)   = False
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

trkRexM :: Monad m => Rex -> m ()
trkRexM rex = do
    let !() = doTrkRex rex ()
    pure ()

trk :: Any -> a -> a
trk = doTrk

--------------------------------------------------------------------------------

apple :: Sire -> [Sire] -> Sire
apple = foldl' A

apple_ :: [Sire] -> Sire
apple_ []     = error "apple_ given nothing to work with"
apple_ (f:xs) = apple f xs

traceSire' :: Text -> Sire -> a -> a
traceSire' context sire result =
    doTrkRex it result
  where
    it = N OPEN "#" [T WORD context Nothing]
       $ Just
       $ sireRex sire

traceSire :: Text -> Sire -> a -> a
traceSire _context _sire result = result
-- traceSire = traceSire'

traceSireId :: Text -> Sire -> Sire
traceSireId context sire = traceSire' context sire sire

_traceSireId :: Text -> Sire -> Sire
_traceSireId _context sire = sire
