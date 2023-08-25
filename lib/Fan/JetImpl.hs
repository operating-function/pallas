-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- TODO: =VCHUNKS

{-# OPTIONS_GHC -Wall    #-}
{-# OPTIONS_GHC -Werror  #-}

module Fan.JetImpl (installJetImpls, jetImpls, doTrk) where

import Control.Parallel
import Data.Bits
import Data.Maybe
import Fan.Convert
import Fan.Eval
import Fan.Jets
import PlunderPrelude
import Foreign.ForeignPtr
import Foreign.Storable

import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.Vector             ((//))
import Foreign.Marshal.Alloc   (allocaBytes)
import Foreign.Ptr             (castPtr)
import GHC.Exts                (Word(..))
import Jelly.Fast.FFI          (c_jet_blake3)
import Unsafe.Coerce           (unsafeCoerce)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Vector            as V
import qualified Fan.Prof               as Prof

--------------------------------------------------------------------------------

{-
    Call this immediatly on executable startup.
-}
installJetImpls :: IO ()
installJetImpls = writeIORef vJetImpl jetImpls

--------------------------------------------------------------------------------

jetImpls :: Map Text Jet
jetImpls = mapFromList
  [ ( "_PinItem"    , pinItemJet )
  , ( "_Trk"        , trkJet     )
  , ( "_Seq"        , seqJet     )
  , ( "_If"         , ifJet      )
  , ( "_Bit"        , bitJet     )
  , ( "_Not"        , notJet     )
  , ( "_And"        , andJet     )
  , ( "_Or"         , orJet      )
  , ( "_IsNat"      , isNatJet   )
  , ( "_Dec"        , decJet     )
  , ( "_Add"        , addJet     )
  , ( "_Mul"        , mulJet     )
  , ( "_Sub"        , subJet     )
  , ( "_Bex"        , bexJet     )
  , ( "_Div"        , divJet     )
  , ( "_Mod"        , modJet     )
  , ( "_Lsh"        , lshJet     )
  , ( "_Rsh"        , rshJet     )
  , ( "_Met"        , metJet     )
  , ( "_Mix"        , mixJet     )
  , ( "_Dis"        , disJet     )
  , ( "_Con"        , conJet     )
  , ( "_Cmp"        , cmpJet     )
  , ( "_Eql"        , eqlJet     )
  , ( "_IsZero"     , isZeroJet  )
  , ( "_Neq"        , neqJet     )
  , ( "_Lte"        , lteJet     )
  , ( "_Lth"        , lthJet     )
  , ( "_Gth"        , gthJet     )
  , ( "_Gte"        , gteJet     )
  , ( "_Idx"        , idxJet     )
  , ( "_Get"        , getJet     )
  , ( "_Len"        , lenJet     )
  , ( "_Weld"       , vweldJet   )
  , ( "_Map"        , vmapJet    )
  , ( "_Put"        , vputJet    )
  , ( "_Mut"        , vmutJet    )
  , ( "_Switch"     , vswitchJet )
  , ( "_Rev"        , vrevJet    )
  , ( "rowCons"     , vconsJet   )
  , ( "rowSnoc"     , vsnocJet   )
  , ( "sum"         , vsumJet    )
  , ( "take"        , vtakeJet   )
  , ( "drop"        , vdropJet   )
  , ( "cat"         , vcatJet    )
  , ( "zip"         , vzipJet    )
  , ( "listToRow"   , listToRowJet )
  , ( "listToRowReversed"   , listToRowReversedJet )
  , ( "unfoldr"     , unfoldrJet )

  , ( "isDigit"     , isDigitJet )

  , ( "padWeld"     , padWeldJet )
  , ( "padCat"      , padCatJet  )
  , ( "padFlat"     , padFlatJet )

  , ( "isBar"       , isBarJet   )
  , ( "barIdx"      , bIdxJet    )
  , ( "barWeld"     , barWeldJet )
  , ( "barCat"      , barCatJet  )
  , ( "barFlat"     , barFlatJet )
  , ( "barElemIndexOff", barElemIndex )
  , ( "barElemIndexEnd", barElemIndexEndJet )
  , ( "w32"         , w32Jet     )
  , ( "add32"       , add32Jet   )
  , ( "mul32"       , mul32Jet   )
  , ( "div32"       , div32Jet   )
  , ( "and32"       , and32Jet   )
  , ( "or32"        , or32Jet    )
  , ( "xor32"       , xor32Jet   )
  , ( "lsh32"       , lsh32Jet   )
  , ( "rsh32"       , rsh32Jet   )
  , ( "sub32"       , sub32Jet   )
  , ( "ror32"       , ror32Jet   )
  , ( "rol32"       , rol32Jet   )
  , ( "w64"         , w64Jet     )
  , ( "add64"       , add64Jet   )
  , ( "mul64"       , mul64Jet   )
  , ( "div64"       , div64Jet   )
  , ( "and64"       , and64Jet   )
  , ( "or64"        , or64Jet    )
  , ( "xor64"       , xor64Jet   )
  , ( "lsh64"       , lsh64Jet   )
  , ( "rsh64"       , rsh64Jet   )
  , ( "sub64"       , sub64Jet   )
  , ( "ror64"       , ror64Jet   )
  , ( "rol64"       , rol64Jet   )
  , ( "iDiv64"      , iDiv64Jet  )
  , ( "implode"     , implodeJet )
  , ( "barDrop"     , barDropJet )
  , ( "barTake"     , barTakeJet )
  , ( "barLen"      , barLenJet  )
  , ( "natBar"      , natBarJet  )
  , ( "barNat"      , barNatJet )
  , ( "barIsEmpty"  , barIsEmptyJet )
  , ( "setSing"     , setSingletonJet )
  , ( "setIns"      , setInsJet  )
  , ( "setDel"      , setDelJet  )
  , ( "setMin"      , setMinJet  )
  , ( "setLen"      , setLenJet  )
  , ( "setWeld"     , setWeldJet )
  , ( "setCatRowAsc", setCatRowAscJet )
  , ( "setHas"      , setHasJet  )
  , ( "setTake"     , setTakeJet )
  , ( "setDrop"     , setDropJet )
  , ( "setIsEmpty"  , setIsEmptyJet )
  , ( "setSplitAt"  , setSplitAtJet )
  , ( "setSplitLT"  , setSplitLTJet )
  , ( "setIntersect", setIntersectionJet )
  , ( "setSub"      , setSubJet )
  , ( "tabSwitch"   , tabSwitchJet    )
  , ( "tabSing"     , tabSingletonJet )
  , ( "isTab"       , isTabJet )
  , ( "tabIdx"      , tabIdxJet )
  , ( "tabIns"      , tabInsJet )
  , ( "tabElemIdx"  , tabElemIdxJet )
  , ( "_TabLen"      , tabLenJet )
  , ( "tabToPairs"  , tabToPairsJet )
  , ( "tabFromPairs", tabFromPairsJet )
  , ( "tabToPairList" , tabToPairListJet )
  , ( "_TabLookup"    , tabLookupJet )
  , ( "tabSplitAt"  , tabSplitAtJet )
  , ( "tabSplitLT"  , tabSplitLTJet )
  , ( "tabMapWithKey" , tabMapWithKeyJet )
  , ( "tabUnionWith", tabUnionWithJet )
  , ( "tabMinKey"   , tabMinKeyJet )
  , ( "tabFoldlWithKey" , tabFoldlWithKeyJet )
  , ( "tabAlter"    , tabAlterJet )
  , ( "_TabHas"      , tabHasKeyJet )
  , ( "_TabKeys"  , tabKeysSetJet )
  , ( "_TabKeysRow"  , tabKeysRowJet )
  , ( "_TabVals"     , tabValsJet )
  , ( "blake3"      , blake3Jet )

  -- par
  , ( "par"         , parJet )
  , ( "pseq"        , pseqJet )

  -- par
  , ( "_DataTag"        , dataTagJet )
  , ( "_TypeTag"        , typeTagJet )
  ]

--------------------------------------------------------------------------------

ifJet :: Jet
ifJet _ env = if toBit(env.!1) then env.!2 else env.!3

isNatJet :: Jet
isNatJet _ env =
    case env.!1 of
        NAT{} -> NAT 1
        _     -> NAT 0

eqlJet :: Jet
eqlJet _ env =
    fromBit $ (fastValEq (env.!1) (env.!2))

neqJet :: Jet
neqJet _ env =
    fromBit $ not $ (fastValEq (env.!1) (env.!2))

isZeroJet :: Jet
isZeroJet _ env = case env.!1 of
  NAT 0 -> NAT 1
  _     -> NAT 0

doTrk :: Fan -> a -> a
doTrk msg val = unsafePerformIO do
    trk <- readIORef vTrkFan
    tag <- evaluate (force msg)
    trk msg

    case trkName tag of
        Nothing -> evaluate val
        Just (encodeUtf8 -> nm) ->
            Prof.withAlwaysTrace nm "trk" do
                evaluate val

-- TODO: (!greenOut (!(readIORef vShowFan) (env.!1))) probably needs to
-- be replaced with something like (!(readIORef vLogFan) LOG_TRK (env.!1)).
--
-- This way the cog-machine can propery re-route this output to the
-- log-files.
trkJet :: Jet
trkJet _ env = doTrk (env.!1) (env.!2)


{-# INLINE ordFan #-}
ordFan :: Ordering -> Nat
ordFan LT = 0
ordFan EQ = 1
ordFan GT = 2

cmpJet :: Jet
cmpJet _ env = NAT $ ordFan $ compare (env.!1) (env.!2)

lthJet :: Jet
lthJet _ env = if ((env.!1) <  (env.!2)) then NAT 1 else NAT 0

gthJet :: Jet
gthJet _ env = if ((env.!1) >  (env.!2)) then NAT 1 else NAT 0

lteJet :: Jet
lteJet _ env = if ((env.!1) <= (env.!2)) then NAT 1 else NAT 0

gteJet :: Jet
gteJet _ env = if ((env.!1) >= (env.!2)) then NAT 1 else NAT 0

bexJet :: Jet
bexJet _ env = NAT (bex $ toNat (env.!1))

modJet :: Jet
modJet _ env = NAT (toNat(env.!1) `mod` toNat(env.!2))

addJet :: Jet
addJet _ env = NAT (toNat(env.!1) + toNat(env.!2))

lshJet :: Jet
lshJet _ env =
    let xv = toNat(env.!1)
        yv = toNat(env.!2)
    in
        if yv > maxInt
        then error "TODO:lsh with huge offset"
        else NAT (xv `shiftL` (fromIntegral yv :: Int))

rshJet :: Jet
rshJet _ env =
    let xv = toNat(env.!1)
        yv = toNat(env.!2)
    in
        if yv > maxInt
        then error "TODO: rsh with huge offset"
        else NAT (xv `shiftR` (fromIntegral yv :: Int))

metJet :: Jet
metJet _ env =
    let x = toNat(env.!1)
    in NAT $ natBitWidth x

pinItemJet :: Jet
pinItemJet _ env =
    case env.!1 of
        PIN p -> p.item
        _     -> NAT 0

decJet :: Jet
decJet _ env =
    case env.!1 of
        NAT 0 -> NAT 0
        NAT n -> NAT (n-1)
        _     -> NAT 0

seqJet :: Jet
seqJet _ env = env.!1 `seq` env.!2

bitJet :: Jet
bitJet _ env =
    case env.!1 of
        NAT 0 -> NAT 0
        NAT _ -> NAT 1
        _     -> NAT 0

notJet :: Jet
notJet _ env =
    case env.!1 of
        NAT (NatS# 0##)       -> NAT (NatS# 1##)
--      NAT (NatJ# (EXO 0 _)) -> error "invalid nat"
--      NAT (NatJ# (EXO 1 _)) -> error "invalid nat"
        NAT _                 -> NAT (NatS# 0##)
        _                     -> NAT 1

andJet :: Jet
andJet _ env = fromBit (toBit(env.!1) && toBit(env.!2))

orJet :: Jet
orJet _ env = fromBit (toBit(env.!1) || toBit(env.!2))

mulJet :: Jet
mulJet _ env = NAT (toNat(env.!1) * toNat(env.!2))

mixJet :: Jet
mixJet _ env = NAT (toNat(env.!1) `xor` toNat(env.!2))

conJet :: Jet
conJet _ env = NAT (toNat(env.!1)  .&.  toNat(env.!2))

disJet :: Jet
disJet _ env = NAT (toNat(env.!1)  .|. toNat(env.!2))

divJet :: Jet
divJet _ env =
    let yv = toNat (env.!2)
    in if (yv == 0)
       then NAT 0
       else NAT (toNat(env.!1) `div` yv)

subJet :: Jet
subJet _ env =
    let (x,y) = (toNat(env.!1), toNat(env.!2))
    in NAT (if y>x then 0 else (x-y))

vcatJet :: Jet
vcatJet f env = orExec (f env) do
      vs <- getRow (env.!1)
      xs <- for vs getRow
      pure $ ROW $ concat xs

vzipJet :: Jet
vzipJet f env = orExec (f env) do
    as <- getRow (env.!1)
    bs <- getRow (env.!2)
    pure $ ROW $ V.zipWith v2 as bs

vrevJet :: Jet
vrevJet f env = orExec (f env) do
      (ROW . V.reverse) <$> getRow (env.!1)

listToRow :: Fan -> Maybe (Vector Fan)
listToRow input = V.unfoldrM build input
  where
    build :: Fan -> Maybe (Maybe (Fan, Fan))
    build (NAT _) = Just $ Nothing
    build (ROW r) | V.length r == 2 = Just $ Just (r V.! 0, r V.! 1)
    build _ = Nothing

listToRowJet :: Jet
listToRowJet f env = orExec (f env) (ROW <$> listToRow (env.!1))

listToRowReversedJet :: Jet
listToRowReversedJet f env = orExec (f env)
                             ((ROW . V.reverse) <$> listToRow (env.!1))

unfoldrJet :: Jet
unfoldrJet f env = orExecTrace "unfoldr" (f env)
                   (ROW <$> V.unfoldrM build (env.!2))
  where
    fun = env.!1
    build val = fromNoun @(Maybe (Fan, Fan)) (fun %% val)

-- TODO Just don't do this, use bars instead of rows of bytes.
--
-- We don't accept 0 bytes, since their behavior in the plunder
-- implementation is weird (silently dropped)
--
-- TODO Converting from a vector to a list to a bytestring to an atom
-- is stupid we should directly implement `Vector U8 -> Nat`.
implodeJet :: Jet
implodeJet f env = orExec (f env) $ do
      vs <- getRow (env.!1)
      bs <- for vs \case
          (NAT n) | n>0 && n<256 -> Just (fromIntegral n)
          _                      -> Nothing
      pure $ NAT $ bytesNat $ pack $ toList bs

barDropJet :: Jet
barDropJet f env = orExec (f env) $ do
    let n = toNat (env.!1)
    b <- getBar (env.!2)
    pure $ BAR $
        if (n >= fromIntegral (length b)) -- Prevent Int overflow
        then mempty
        else drop (fromIntegral n) b

barTakeJet :: Jet
barTakeJet f env = orExec (f env) $ do
    let n = toNat (env.!1)
    b <- getBar (env.!2)

    pure $ BAR $
        if (n >= fromIntegral (length b)) -- Prevent Int overflow
        then b
        else take (fromIntegral n) b

barLenJet :: Jet
barLenJet f env = orExec (f env) $ do
    b <- getBar (env.!1)
    pure $ NAT $ fromIntegral $ length b

natBarJet :: Jet
natBarJet _ env = BAR $ natBytes $ toNat(env.!1)

barNatJet :: Jet
barNatJet f e = orExec (f e) $ do
  b <- getBar (e.!1)
  pure $ NAT $ bytesNat b

barIsEmptyJet :: Jet
barIsEmptyJet f e = orExec (f e) $ do
  b <- getBar (e.!1)
  pure $ fromBit $ null b

idxJet, getJet :: Jet
idxJet _ env = fanIdx (toNat (env.!1)) (env.!2)
getJet _ env = fanIdx (toNat (env.!2)) (env.!1)

-- Number of arguments applied to head.
fanLength :: Fan -> Int
fanLength = \case
    ROW x   -> length x
    KLO _ t -> fanLength (t.!0) + (sizeofSmallArray t - 1)
    TAb{}   -> 1 -- always (keys args)
    NAT{}   -> 0
    BAR{}   -> 0
    SET{}   -> 0
    FUN{}   -> 0
    REX{}   -> 0
    PIN{}   -> 0
    COw{}   -> 0

lenJet :: Jet
lenJet _ env = NAT $ fromIntegral $ fanLength (env.!1)

-- TODO: vsplice

vweldJet :: Jet
vweldJet f env =
    orExec (f env) (vweld <$> getRow (env.!1) <*> getRow (env.!2))
  where
    vweld :: Vector Fan -> Vector Fan -> Fan
    vweld x y = ROW (x ++ y)

vmapJet :: Jet
vmapJet f env =
    orExec (f env) (vmap (env.!1) <$> getRow (env.!2))
  where
    vmap :: Fan -> Vector Fan -> Fan
    vmap fun vec = ROW $ fmap (fun %%) vec

vconsJet :: Jet
vconsJet f env =
    orExec (f env) (vcons (env.!1) <$> getRow (env.!2))
  where
    vcons :: Fan -> Vector Fan -> Fan
    vcons hed vec = ROW (V.cons hed vec)

vsnocJet :: Jet
vsnocJet f env =
    orExec (f env) do
        row <- getRow (env.!1)
        pure (vmap row (env.!2))
  where
    vmap :: Vector Fan -> Fan -> Fan
    vmap vec tel = ROW (V.snoc vec tel)

vsumJet :: Jet
vsumJet f env = orExec (f env) (vsum <$> getRow (env.!1))
  where
    vsum :: Vector Fan -> Fan
    vsum s = NAT $ foldr (\fan n -> n + toNat fan) 0 s

  -- TODO: vfind

vput :: Nat -> Fan -> Vector Fan -> Fan
vput ix vl rw =
    let !siz = fromIntegral (length rw)
    in ROW $ if (ix >= siz)
             then rw
             else rw // [(fromIntegral ix, vl)]

vputJet :: Jet
vputJet f env =
    orExec (f env) do
        rw <- getRow (env.!1)
        let ix = toNat (env.!2)
        let vl = env.!3
        pure (vput ix vl rw)

vmutJet :: Jet
vmutJet f env =
    orExec (f env) do
        let ix = toNat (env.!1)
            vl = (env.!2)
        rw <- getRow (env.!3)
        pure (vput ix vl rw)

-- Just jetting this so that it will show up "NOT MATCHED" if the hash
-- is wrong.
vswitchJet :: Jet
vswitchJet f env =
    orExec (f env) (vtake (toNat (env.!1)) (env.!2) <$> getRow (env.!3))
  where
    vtake :: Nat -> Fan -> Vector Fan -> Fan
    vtake i fb vec =
        if (i >= fromIntegral (length vec))
        then fb
        else vec V.! fromIntegral i

vtakeJet :: Jet
vtakeJet f env =
    orExec (f env) (vtake (toNat (env.!1)) <$> getRow (env.!2))
  where
    vtake :: Nat -> Vector Fan -> Fan
    vtake n vec =
        let siz = fromIntegral (length vec)
        in ROW $ if (n >= siz)
                 then vec
                 else V.take (fromIntegral n) vec

vdropJet :: Jet
vdropJet f env =
    orExec (f env) (vdrop (toNat (env.!1)) <$> getRow (env.!2))
  where
    vdrop :: Nat -> Vector Fan -> Fan
    vdrop n vec =
      let siz = fromIntegral (length vec)
      in ROW $ if (n >= siz)
               then V.empty
               else V.drop (fromIntegral n) vec

bIdxJet :: Jet
bIdxJet f env =
    orExec (f env) (bidx (toNat (env.!1)) <$> getBar (env.!2))
  where
    bidx :: Nat -> ByteString -> Fan
    bidx n bs =
        let siz = fromIntegral (length bs)
        in NAT $ if (n >= siz)
                 then 0
                 else fromIntegral $ BS.index bs $ fromIntegral n

barCatJet :: Jet
barCatJet f env =
    orExecTrace "barCat" (f env) $ do
        vs <- getRow (env.!1)
        bs <- traverse getBar vs
        pure $ BAR $ concat bs

isBarJet :: Jet
isBarJet _ env =
    case env.!1 of
        BAR _ -> NAT 1
        _     -> NAT 0

barFlatJet :: Jet
barFlatJet _ env =
    BAR $ toStrict $ toLazyByteString $ go $ (env.!1)
  where
    go (BAR b) = byteString b
    go (ROW r) = concat (go <$> r)
    go (TAb r) = concat (go <$> toList r)
    go FUN{}   = mempty
    go NAT{}   = mempty
    go _       = error "TODO"

getInt :: Fan -> Maybe Int
getInt (NAT n) | n < maxInt = Just (fromIntegral n)
getInt _                    = Nothing

-- TODO Is `BS.drop` O(n) now?
barElemIndex :: Jet
barElemIndex f env =
    orExec (f env) (exe <$> getByte (env.!1)
                        <*> getInt (env.!2)
                        <*> getBar (env.!3))
  where
    exe :: Word8 -> Int -> ByteString -> Fan
    exe byte off bar =
        NAT $ fromIntegral $
            case BS.elemIndex byte (drop off bar) of
                Nothing -> length bar
                Just ix -> ix+off

barElemIndexEndJet :: Jet
barElemIndexEndJet f env =
    orExec (f env) (exe <$> getByte (env.!1) <*> getBar (env.!2))
  where
    exe :: Word8 -> ByteString -> Fan
    exe byte bar = case BS.elemIndexEnd byte bar of
                     Nothing -> NAT 0
                     Just ix -> (NAT 0) %% (NAT $ fromIntegral ix)

{-
padFlatFill :: Int -> UMVector Word -> [Nat] -> IO Fan
padFlatFill wordLength bufr = do
    let go !used !idx !word []     = pure ()
        go !used !idx !word (x:xs) = do
            let (used, word) = mix (used, word) x

    if wordLength == 1 then
        fromIntegral <$> peek bufr
    else
        _
  where
    finalize :: (Int, Word) -> Word
    finalize (used, acc) =
        fromIntegral ((1 `shiftL` used) .|. acc)

    mix :: (Int, Word) -> Nat -> (Int, Word)
    mix (used, acc) (fromIntegral -> new) =
        let
            end = fromIntegral (wordBitWidth new) - 1
        in
            ( used + end
            , acc .|. (clearBit new end `shiftL` used)
            )

padFlatJet :: Jet
padFlatJet _ e =
    unsafePerformIO do
        buf <- mallocBytes (8*wordWidth)
        padFlatFill wordWidth buf (padFlatSeq arg)
  where
    arg = (e.!1)

    bitWidth  = 1 + padFlatBits arg
    wordWidth = bitWidth `divUp` 64
    divUp x y = (x `div` y) + (if x `mod` y == 0 then 0 else 1)

    padFlatBits :: Fan -> Int
    padFlatBits fan = sum ((\x -> natBitWidth x - 1) <$> padFlatSeq fan)
-}

-- TODO: Stub (finish implementing the much faster approach above)
padFlatJet :: Jet
padFlatJet _ e = pcat $ padFlatSeq (e.!1)
  where
    pcat vs = NAT $ foldl' padWeld 1 vs

padFlatSeq :: Fan -> [Nat]
padFlatSeq =
    \top -> go top []
  where
    go item acc = case item of
        NAT 0   -> 1 : acc
        NAT n   -> n : acc
        TAb xs  -> foldr go acc (toList xs)
        ROW xs  -> foldr go acc (toList xs)
        KLO _ x -> foldr go acc (drop 1 $ toList x)
        _       -> 1 : acc


{-
    p#111 p#110 -> p#111110

    0b1111 0b1011 -> 0b1011111

    (clearBit 3 0b1111) .|. (0b1011 << 3)

    - where 3 is (bitWidth(0b1111) - 1)

    -- TODO: This crashes if gen a zero input.  0 is not a valid pad, but
    -- what should we do in this case?  Abort and fallback to raw PLAN exe?
-}
padWeld :: Nat -> Nat -> Nat
padWeld x y = (x `clearBit` end) .|. (y `shiftL` end)
  where
    end :: Int
    end = (natBitWidth x - 1)

isDigitJet :: Jet
isDigitJet _ e =
    case (e.!1) of
      NAT (NatS# xu) ->
          let x = W# xu
          in if x>=48 && x<=57
             then NAT (NatS# 1##)
             else NAT (NatS# 0##)
      _ ->
          NAT 0

toPad :: Fan -> Nat
toPad (NAT 0) = 1
toPad (NAT n) = n
toPad _       = 1

padWeldJet :: Jet
padWeldJet _ e = (NAT $ padWeld (toPad(e.!1)) (toPad(e.!2)))

-- TODO Do this using a mutable buffer (like in Jam)
padCatJet :: Jet
padCatJet f e =
    orExecTrace "padCat" (f e)
        (pcat <$> getRow (e.!1))
  where
    pcat vs = NAT $ V.foldl (\a i -> padWeld a (toPad i)) 1 vs

barWeldJet :: Jet
barWeldJet f e =
    orExecTrace "barWeld" (f e)
        (bweld <$> getBar (e.!1) <*> getBar (e.!2))
  where
    bweld :: ByteString -> ByteString -> Fan
    bweld a b = BAR (a <> b)

blake3Jet :: Jet
blake3Jet f e =
    orExecTrace "blake3" (f e) do
        blake3 <$> getBar (e.!1)
  where
    blake3 bar =
        unsafePerformIO $
        allocaBytes 32 \outbuf ->
        BS.unsafeUseAsCStringLen bar \(byt, wid) -> do
            c_jet_blake3 (castPtr outbuf) (fromIntegral wid) (castPtr byt)
            res <- BS.packCStringLen (outbuf, 32)
            pure (BAR res)

setSingletonJet :: Jet
setSingletonJet _ e = SET $ S.singleton (e.!1)

setInsJet :: Jet
setInsJet f e =
    orExecTrace "setIns" (f e) (i (e.!1) <$> getSet (e.!2))
  where
    i :: Fan -> Set Fan -> Fan
    i n s = SET (S.insert n s)

setDelJet :: Jet
setDelJet f e =
    orExecTrace "setDel" (f e) (d (e.!1) <$> getSet (e.!2))
  where
    d :: Fan -> Set Fan -> Fan
    d n s = SET (S.delete n s)

setMinJet :: Jet
setMinJet f e =
    orExecTrace "setMin" (f e) (smin <$> getSet (e.!1))
  where
    smin :: Set Fan -> Fan
    smin s = case S.lookupMin s of
      Nothing -> NAT 0
      Just m  -> m

setLenJet :: Jet
setLenJet f e =
    orExecTrace "setLen" (f e) (clen <$> getSet (e.!1))
  where
    clen :: Set Fan -> Fan
    clen = NAT . fromIntegral . S.size

setWeldJet :: Jet
setWeldJet f e =
    orExecTrace "setWeld" (f e) (u <$> getSet (e.!1) <*> getSet (e.!2))
  where
    u :: Set Fan -> Set Fan -> Fan
    u a b = SET (S.union a b)

setCatRowAscJet :: Jet
setCatRowAscJet f e = orExecTrace "setCatRowAsc" (f e) do
  r <- getRow (e.!1)
  sets <- filter (not . S.null) <$> traverse getSet r
  guard (isAsc $ V.toList sets)
  pure $ SET $ S.fromDistinctAscList $ concat $ map toList sets
  where
    isAsc []       = True
    isAsc [_]      = True
    isAsc (x:y:zs) = (S.findMax x < S.findMin y) && isAsc (y:zs)


setHasJet :: Jet
setHasJet f e =
    orExecTrace "setHas" (f e) (has (e.!1) <$> getSet (e.!2))
  where
    has :: Fan -> Set Fan -> Fan
    has n s = fromBit $ S.member n s

setTakeJet :: Jet
setTakeJet f e =
    orExecTrace "setTake" (f e) (doTake (toNat(e.!1)) <$> getSet (e.!2))
  where
    doTake :: Nat -> Set Fan -> Fan
    doTake n s = SET (S.take (fromIntegral n) s)

setDropJet :: Jet
setDropJet f e =
    orExecTrace "setDrop" (f e) (doDrop (toNat(e.!1)) <$> getSet (e.!2))
  where
    doDrop :: Nat -> Set Fan -> Fan
    doDrop n s = SET (S.drop (fromIntegral n) s)

setIsEmptyJet :: Jet
setIsEmptyJet f e =
    orExecTrace "setIsEmpty" (f e) (doIs <$> getSet (e.!1))
  where
    doIs :: Set Fan -> Fan
    doIs s = fromBit $ S.null s

setSplitAtJet :: Jet
setSplitAtJet f e =
    orExecTrace "setSplitAt" (f e)
                (doSplitAt (toNat(e.!1)) <$> getSet (e.!2))
  where
    doSplitAt :: Nat -> Set Fan -> Fan
    doSplitAt n s = let (a, b) = S.splitAt (fromIntegral n) s
                    in ROW $ V.fromList [SET a, SET b]

setSplitLTJet :: Jet
setSplitLTJet f e =
    orExecTrace "setSplitLT" (f e)
                (doSplitLT (e.!1) <$> getSet (e.!2))
  where
    doSplitLT :: Fan -> Set Fan -> Fan
    doSplitLT n s = let (a, b) = S.spanAntitone (< n) s
                    in ROW $ V.fromList [SET a, SET b]

setIntersectionJet :: Jet
setIntersectionJet f e =
    orExecTrace "setIntersection" (f e)
                (doIntersection <$> getSet (e.!1) <*> getSet (e.!2))
  where
    doIntersection :: Set Fan -> Set Fan -> Fan
    doIntersection a b = SET (S.intersection a b)

setSubJet :: Jet
setSubJet f e =
    orExecTrace "setSub" (f e)
                (doDifference <$> getSet (e.!1) <*> getSet (e.!2))
  where
    doDifference :: Set Fan -> Set Fan -> Fan
    doDifference a b = SET (S.difference a b)

tabSingletonJet :: Jet
tabSingletonJet _ e = TAb $ M.singleton (e.!1) (e.!2)

-- An empty set is a tab, but the runtime always makes sure to represent
-- empty sets as tabs.
isTabJet :: Jet
isTabJet _ e =
    case (e.!1) of
        TAb{} -> NAT 1
        _     -> NAT 0

-- Just jetting this so that it will show up "NOT MATCHED" if the hash
-- is wrong.
tabSwitchJet :: Jet
tabSwitchJet f e =
    orExecTrace "tabSwitch" (f e) (tswitch (e.!1) (e.!2) <$> getTab (e.!3))
  where
    tswitch key fal tab =
        case lookup key tab of
            Just x  -> x
            Nothing -> fal

tabIdxJet :: Jet
tabIdxJet f e =
    orExecTrace "tabIdx" (f e) (tidx (e.!1) <$> getTab (e.!2))
  where
    tidx k m = case M.lookup k m of
      Nothing -> NAT 0
      Just x  -> x

tabInsJet :: Jet
tabInsJet f e =
    orExecTrace "tabIns" (f e) (tmut (e.!1) (e.!2) <$> getTab (e.!3))
  where
    tmut :: Fan -> Fan -> Map Fan Fan -> Fan
    tmut k v t = TAb $ M.insert k v t

tabElemIdxJet :: Jet
tabElemIdxJet f e =
    orExecTrace "tabElemIdx" (f e) (telem (toNat(e.!1)) <$> getTab (e.!2))
  where
    telem :: Nat -> Map Fan Fan -> Fan
    telem i m = let n = fromIntegral i
                in if n >= M.size m then NAT 0
                   else let (k, v) = M.elemAt n m
                        in ROW $ V.fromList [k, v]

tabLenJet :: Jet
tabLenJet f e =
    orExecTrace "tabLen" (f e) (tlen <$> getTab (e.!1))
  where
    tlen :: Map Fan Fan -> Fan
    tlen = NAT . fromIntegral . M.size

tabToPairsJet :: Jet
tabToPairsJet f e =
    orExecTrace "tabToPairs" (f e) (toP <$> getTab (e.!1))
  where
    toP :: Map Fan Fan -> Fan
    toP tab = ROW $ V.fromListN (length tab) $ map v2' $ mapToList tab

tabToPairListJet :: Jet
tabToPairListJet f e =
    orExecTrace "tabToPairList" (f e) (go . mapToList <$> getTab (e.!1))
  where
    go []          = NAT 0
    go ((k,v):kvs) = v2 (v2 k v) (go kvs)

{-# INLINE v2 #-}
v2 :: Fan -> Fan -> Fan
v2 x y = ROW $ V.fromListN 2 [x,y]

{-# INLINE v2' #-}
v2' :: (Fan, Fan) -> Fan
v2' (x,y) = ROW $ V.fromListN 2 [x,y]

tabFromPairsJet :: Jet
tabFromPairsJet f e =
    orExecTrace "tabFromPairs" (f e) (toP <$> getPairs (e.!1))
  where
    toP :: [(Fan, Fan)] -> Fan
    toP = TAb . mapFromList

    getPairs :: Fan -> Maybe [(Fan, Fan)]
    getPairs x = do
        row <- getRow x
        res <- traverse getPair row
        pure (toList res)

    getPair :: Fan -> Maybe (Fan, Fan)
    getPair x = do
        vs <- getRow x
        guard (length vs == 2)
        Just (vs V.! 0, vs V.! 1)

tabLookupJet :: Jet
tabLookupJet f e =
    orExecTrace "tabLookup" (f e)
                (doLookup (e.!1) <$> getTab (e.!2))
  where
    doLookup :: Fan -> Map Fan Fan -> Fan
    doLookup n t = case M.lookup n t of
      Nothing  -> NAT 0
      Just fun -> NAT 0 %% fun

tabSplitAtJet :: Jet
tabSplitAtJet f e =
    orExecTrace "tabSplitAt" (f e)
                (doSplitAt (toNat(e.!1)) <$> getTab(e.!2))
  where
    doSplitAt :: Nat -> Map Fan Fan -> Fan
    doSplitAt n s = let (a, b) = M.splitAt (fromIntegral n) s
                    in ROW $ V.fromList [TAb a, TAb b]

tabSplitLTJet :: Jet
tabSplitLTJet f e =
    orExecTrace "tabSplitLT" (f e)
                (doSplitLT (e.!1) <$> getTab (e.!2))
  where
    doSplitLT :: Fan -> Map Fan Fan -> Fan
    doSplitLT n s = let (a, b) = M.spanAntitone (< n) s
                    in ROW $ V.fromList [TAb a, TAb b]

tabMapWithKeyJet :: Jet
tabMapWithKeyJet f e =
    orExecTrace "tabMapWithKey" (f e)
                (doMap <$> (Just $ e.!1) <*> getTab (e.!2))
  where
    doMap :: Fan -> Map Fan Fan -> Fan
    doMap fun a = TAb $ M.mapWithKey (apply fun) a

    apply :: Fan -> Fan -> Fan -> Fan
    apply fun k v = fun %% k %% v

tabUnionWithJet :: Jet
tabUnionWithJet f e =
    orExecTrace "tabUnionWith" (f e)
                (doUnionWith <$> (Just $ e.!1) <*> getTab (e.!2) <*> getTab (e.!3))
  where
    doUnionWith :: Fan -> Map Fan Fan -> Map Fan Fan -> Fan
    doUnionWith fun a b = TAb $ M.unionWith (apply fun) a b

    apply :: Fan -> Fan -> Fan -> Fan
    apply fun a b = fun %% a %% b

tabMinKeyJet :: Jet
tabMinKeyJet f e =
    orExecTrace "tabMin" (f e) (tmin <$> getTab (e.!1))
  where
    tmin :: Map Fan Fan -> Fan
    tmin s = case M.lookupMin s of
      Nothing     -> NAT 0
      Just (k, _) -> k

tabFoldlWithKeyJet :: Jet
tabFoldlWithKeyJet f e =
    orExecTrace "tabFoldlWithKey" (f e) $ do
      tab <- getTab $ e.!3
      let fun = e.!1
          initial = e.!2
      let wrapFun a k v = fun %% a %% k %% v
      pure $ M.foldlWithKey' wrapFun initial tab

tabAlterJet :: Jet
tabAlterJet f e =
    orExecTrace "tabAlter" (f e) (alt (e.!1) (e.!2) <$> getTab (e.!3))
  where
    alt :: Fan -> Fan -> Map Fan Fan -> Fan
    alt fun key m = TAb $ M.alter (someAsMaybe . wrap fun) key m

    -- Figuring this out is the next big thing
    wrap :: Fan -> Maybe Fan -> Fan
    wrap fun (Nothing) = fun %% NAT 0
    wrap fun (Just x)  = fun %% (NAT 0 %% x)

    someAsMaybe :: Fan -> Maybe Fan
    someAsMaybe = \case
      NAT _ -> Nothing
      x     -> Just $ snd $ boom x

tabHasKeyJet :: Jet
tabHasKeyJet f e =
    orExecTrace "tabHas" (f e) (hk (e.!1) <$> getTab(e.!2))
  where
    hk :: Fan -> Map Fan Fan -> Fan
    hk k m = case M.member k m of
        False -> NAT 0
        True  -> NAT 1

tabKeysRowJet :: Jet
tabKeysRowJet f e = orExecTrace "_TabKeysRow" (f e) (tk <$> getTab(e.!1))
  where
    tk :: Map Fan Fan -> Fan
    tk = ROW . V.fromList . M.keys

tabKeysSetJet :: Jet
tabKeysSetJet f e = orExecTrace "_TabKeys" (f e) (tk <$> getTab(e.!1))
  where
    tk :: Map Fan Fan -> Fan
    tk = SET . M.keysSet

tabValsJet :: Jet
tabValsJet f e = orExecTrace "tabVals" (f e) (tv <$> getTab(e.!1))
  where
    tv :: Map Fan Fan -> Fan
    tv = ROW . V.fromList . M.elems

typeTagJet :: Jet
typeTagJet _ e =
    case (e.!1) of
        PIN{} -> 0
        FUN{} -> 1
        KLO{} -> 2
        NAT{} -> 3
        BAR{} -> 4
        ROW{} -> 5
        TAb{} -> 6
        COw{} -> 7
        SET{} -> 8
        REX{} -> 1 -- law (TODO: update this when repr changes)

{-
    Returns either a nat, the first element of a closure (the last
    element applied), or 0 (for law/pin).

    The (idx 0) of a tab is the values array.
-}
dataTagJet :: Jet
dataTagJet _ e =
    let v = e.!1 in
    case v of
        ROW r -> if null r then 0 else (r V.! 0) -- app
        KLO{} -> snd $ boom v                    -- app
        TAb{} -> snd $ boom v                    -- app
        PIN{} -> 0 -- pin
        FUN{} -> 0 -- law
        BAR{} -> 0 -- law
        COw{} -> 0 -- law
        SET{} -> 0 -- law
        REX{} -> 0 -- law (TODO: update this when repr changes)
        NAT{} -> v

---------
-- w32 --
---------

w32Jet :: Jet
w32Jet _ env =
    NAT (fromIntegral . w32 $ toNat(env.!1))

w32op :: (Word32 -> Word32 -> Word32) -> Jet
w32op fun _ env = NAT $ fromIntegral $ fun (w32 $ toNat(env.!1)) (w32 $ toNat(env.!2))

add32Jet,mul32Jet,div32Jet,sub32Jet,and32Jet,xor32Jet,or32Jet :: Jet
add32Jet = w32op (+)
mul32Jet = w32op (*)
div32Jet = w32op (div)
sub32Jet = w32op (-)
and32Jet = w32op (.&.)
xor32Jet = w32op xor
or32Jet  = w32op (.|.)

{-# INLINE w32opInt #-}
w32opInt :: (Word32 -> Int -> Word32) -> Jet
w32opInt fun _ env =
    NAT $ fromIntegral $ fun (w32 $ toNat (env.!1))
                             (fromIntegral $ w32 $ toNat (env.!2))

lsh32Jet, rsh32Jet, ror32Jet, rol32Jet :: Jet
rol32Jet = w32opInt rotateL
lsh32Jet = w32opInt shiftL
rsh32Jet = w32opInt shiftR
ror32Jet = w32opInt rotateR

---------
-- w64 --
---------

w64Jet :: Jet
w64Jet _ env =
    NAT (fromIntegral . w64 $ toNat(env.!1))

{-# INLINE w64op #-}
w64op :: (Word64 -> Word64 -> Word64) -> Jet
w64op fun _ env = NAT $ fromIntegral $ fun (w64 $ toNat(env.!1)) (w64 $ toNat(env.!2))

add64Jet,mul64Jet,div64Jet,sub64Jet,and64Jet,xor64Jet,or64Jet :: Jet
add64Jet = w64op (+)
mul64Jet = w64op (*)
div64Jet = w64op (div)
sub64Jet = w64op (-)
and64Jet = w64op (.&.)
xor64Jet = w64op xor
or64Jet  = w64op (.|.)

{-# INLINE w64opInt #-}
w64opInt :: (Word64 -> Int -> Word64) -> Jet
w64opInt fun _ env =
    NAT $ fromIntegral $ fun (w64 $ toNat (env.!1))
                             (fromIntegral $ w64 $ toNat (env.!2))

lsh64Jet, rsh64Jet, ror64Jet, rol64Jet :: Jet
rol64Jet = w64opInt rotateL
lsh64Jet = w64opInt shiftL
rsh64Jet = w64opInt shiftR
ror64Jet = w64opInt rotateR

---------
-- i64 --
---------

i64op :: (Int64 -> Int64 -> Int64) -> Jet
i64op fun _ env = NAT $ i64toNat $ fun (i64 $ toNat(env.!1)) (i64 $ toNat(env.!2))

iDiv64Jet :: Jet
iDiv64Jet = i64op (div)

parJet :: Jet
parJet = unsafePerformIO do
  -- When law profiling is enabled, we disable `par` because actually trying to
  -- create sparks in the haskell interpreter screws up tracing. (par a b)=b is
  -- an entirely semantically valid definition of `par` and is what everything
  -- other than GHC does.
  Prof.lawProfilingEnabled >>= \case
    True  -> pure \_ env -> env.!2
    False -> pure \_ env -> env.!1 `par` env.!2

pseqJet :: Jet
pseqJet _ env = env.!1 `pseq` env.!2

-- Utils -----------------------------------------------------------------------

toBit :: Fan -> Bool
toBit (NAT 0) = False
toBit (NAT _) = True
toBit _       = False

fromBit :: Bool -> Fan
fromBit True  = NAT 1
fromBit False = NAT 0


-- w32 helpers
bex32 :: Nat
bex32 = 2 ^ (32::Nat)

_w32max :: Nat
_w32max = bex32 - 1

maxInt :: Nat
maxInt = fromIntegral (maxBound::Int)

w32 :: Nat -> Word32
w32 x = fromIntegral (x `mod` bex32)

-- TODO: mod of 2^64 DOES NOT WORK!!  Fix it.

w64 :: Nat -> Word64
w64 (NatS# w) = wordToWord64 (W# w)
w64 (NatJ# x) = wordToWord64 (unsafePerformIO $ withForeignPtr x.ptr peek)

-- TODO: Use unboxed.
-- TODO: Explicit cast (they are identical on this arch)
{-# INLINE wordToWord64 #-}
wordToWord64 :: Word -> Word64
wordToWord64 = unsafeCoerce

-- i64 helpers
-- Int<->Word conversions preserve representation, not sign
i64 :: Nat -> Int64
i64 = fromIntegral . w64

i64toNat :: Int64 -> Nat
i64toNat i = fromIntegral (fromIntegral i :: Word64)

bex :: Nat -> Nat
bex n = 2 ^ n

getBar :: Fan -> Maybe ByteString
getBar (BAR b) = Just b
getBar _       = Nothing

getSet :: Fan -> Maybe (Set Fan)
getSet (SET c) = Just c
getSet _       = Nothing

getTab :: Fan -> Maybe (Map Fan Fan)
getTab (TAb b) = Just b
getTab _       = Nothing

orExec :: Fan -> Maybe Fan -> Fan
orExec _  (Just r) = r
orExec fb Nothing  = fb

orExecTrace :: String -> Fan -> Maybe Fan -> Fan
orExecTrace _ fb res = orExec fb res

-- orExecTrace msg xs res = case res of
--     Nothing -> trace (msg <> ".nomatch") (orExec xs res)
--     Just{}  -> trace (msg <> ".match")   (orExec xs res)
