{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{-
    PlanRex is a rex node loaded from a noun.

    It's a lazy, non-validated data structure, so it supports partial
    loading from noun.  It also *remembers* the underlying noun, so
    converting back-and-forth between nouns and PlanRex doesn't requires
    copying data.

    Sire needs to constantly thread rex trees through macros, but also
    needs to be able to pattern match on them.  This representation
    provides a way to do both at the same time, without significant
    overhead.
-}

module Fan.PlanRex
    ( PlanRex(..)
    , PlanRexNode(..)
    , Pex
    , pexNoun, nounPex
    , pattern EVIL, pattern EMBD, pattern LEAF, pattern NODE
    , pattern WORD, pattern TEXT, pattern LINE
    , pattern OPEN, pattern NEST, pattern INFX, pattern PREF, pattern SHUT
    )
where


import PlunderPrelude

import Fan (Fan(NAT,ROW,PIN,FUN), Law(..), Pin(..), Any, (%%), kloList)
import Rex (RuneShape, TextShape)

import qualified Rex as R


-- Types -----------------------------------------------------------------------

data PlanRex = PR
    { n :: !Any
    , v :: !(Maybe PlanRexNode)
    }
  deriving (Eq, Ord, Show, Generic, NFData)

type Pex = PlanRex

data PlanRexNode
    = NODE_ RuneShape Text [Pex] (Maybe Pex)
    | LEAF_ TextShape Text (Maybe Pex)
    | EMBD_ Any
  deriving (Eq, Ord, Show, Generic, NFData)


-- PLAN Constructors -----------------------------------------------------------

embdCnstr :: Any
embdCnstr = 4 %% (0 %% "##EMBD" %% 2 %% 0)

wordCnstr, textCnstr, lineCnstr :: Any
wordCnstr = 4 %% (0 %% "##WORD" %% 3 %% 0)
textCnstr = 4 %% (0 %% "##TEXT" %% 3 %% 0)
lineCnstr = 4 %% (0 %% "##LINE" %% 3 %% 0)

openCnstr, nestCnstr, infxCnstr, prefCnstr, shutCnstr :: Any
openCnstr = 4 %% (0 %% "##OPEN" %% 4 %% 0)
nestCnstr = 4 %% (0 %% "##NEST" %% 4 %% 0)
infxCnstr = 4 %% (0 %% "##INFX" %% 4 %% 0)
prefCnstr = 4 %% (0 %% "##PREF" %% 4 %% 0)
shutCnstr = 4 %% (0 %% "##SHUT" %% 4 %% 0)

leafCnstr :: TextShape -> Any
leafCnstr R.WORD = wordCnstr
leafCnstr R.TEXT = textCnstr
leafCnstr R.LINE = lineCnstr

nodeCnstr :: RuneShape -> Any
nodeCnstr R.OPEN = openCnstr
nodeCnstr R.NEST = nestCnstr
nodeCnstr R.INFX = infxCnstr
nodeCnstr R.PREF = prefCnstr
nodeCnstr R.SHUT = shutCnstr


-- Dumping ---------------------------------------------------------------------

pexNoun :: Pex -> Any
pexNoun = (.n)


-- Loading ---------------------------------------------------------------------

loadLeafConstr :: Fan -> Maybe TextShape
loadLeafConstr (PIN P{item=(FUN L{body=0,args=3,name})}) = do
    case name of
        "##TEXT" -> Just R.TEXT
        "##LINE" -> Just R.LINE
        "##WORD" -> Just R.WORD
        _        -> Nothing
loadLeafConstr _ = Nothing

loadNodeConstr :: Fan -> Maybe RuneShape
loadNodeConstr (PIN P{item=(FUN L{body=0,args=4,name})}) = do
    case name of
        "##OPEN" -> Just R.OPEN
        "##NEST" -> Just R.NEST
        "##INFX" -> Just R.INFX
        "##PREF" -> Just R.PREF
        "##SHUT" -> Just R.SHUT
        _        -> Nothing
loadNodeConstr _ = Nothing

isEmbdConstr :: Fan -> Bool
isEmbdConstr (PIN P{item=(FUN L{body=0,args=2,name})}) = do
    case name of
        "##EMBD" -> True
        _        -> False
isEmbdConstr _ = False

loadHeir :: Any -> Maybe Pex
loadHeir 0 = Nothing
loadHeir x = Just (nounPex x)

nounPex :: Any -> Pex
nounPex noun = PR { n=noun, v=nounPexNode noun }

nounPexNode :: Any -> Maybe PlanRexNode
nounPexNode noun = case kloList noun of
    [(isEmbdConstr -> True), val] ->
        Just $ EMBD_ val

    [(loadLeafConstr -> Just style), NAT text, heir] ->
        Just $ LEAF_ style (natUtf8Exn text)
                           (loadHeir heir)

    [(loadNodeConstr -> Just style), NAT rune, ROW sons, heir] ->
        Just $ NODE_ style (natUtf8Exn rune)
                           (toList (nounPex <$> sons))
                           (loadHeir heir)

    _ ->
        Nothing


-- Constructing ----------------------------------------------------------------

heirNoun :: Maybe Pex -> Any
heirNoun = maybe 0 pexNoun

embd :: Any -> Pex
embd val = PR
    { v = Just (EMBD_ val)
    , n = embdCnstr %% val
    }

leaf :: TextShape -> Text -> Maybe Pex -> Pex
leaf style text heir = PR
    { v = Just (LEAF_ style text heir)
    , n = leafCnstr style %% NAT (utf8Nat text) %% heirNoun heir
    }

node :: RuneShape -> Text -> [Pex] -> Maybe Pex -> Pex
node style rune sons heir = PR
    { v = Just (NODE_ style rune sons heir)
    , n = nodeCnstr style %% NAT (utf8Nat rune)
                          %% ROW (fromList $ map pexNoun sons)
                          %% heirNoun heir
    }


-- Basic Pattern Synonyms ------------------------------------------------------

{-# COMPLETE EVIL, EMBD, LEAF, NODE #-}

pattern EVIL :: Any -> Pex
pattern EVIL val <- PR { v = Nothing, n=val }
  where EVIL val = nounPex val

pattern EMBD :: Any -> Pex
pattern EMBD valu <- PR { v = Just (EMBD_ valu) }
  where EMBD valu = embd valu

pattern LEAF :: TextShape -> Text -> Maybe Pex -> Pex
pattern LEAF style text heir <- PR { v = Just (LEAF_ style text heir) }
  where LEAF style text heir = leaf style text heir

pattern NODE :: RuneShape -> Text -> [Pex] -> Maybe Pex -> Pex
pattern NODE style rune sons heir <- PR { v = Just (NODE_ style rune sons heir) }
  where NODE style rune sons heir = node style rune sons heir


-- High-Level Pattern Synonyms -------------------------------------------------

pattern WORD, TEXT, LINE :: Text -> Maybe Pex -> Pex
pattern WORD txt heir = LEAF R.WORD txt heir
pattern TEXT txt heir = LEAF R.TEXT txt heir
pattern LINE txt heir = LEAF R.LINE txt heir

pattern OPEN, NEST, INFX, PREF, SHUT :: Text -> [Pex] -> Maybe Pex -> Pex
pattern OPEN rune sons heir = NODE R.OPEN rune sons heir
pattern NEST rune sons heir = NODE R.NEST rune sons heir
pattern INFX rune sons heir = NODE R.INFX rune sons heir
pattern PREF rune sons heir = NODE R.PREF rune sons heir
pattern SHUT rune sons heir = NODE R.SHUT rune sons heir
