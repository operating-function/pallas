-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Fan.Jets
    ( vShowFan
    , jetMatch
    , getPin
    , getRow
    , getRowOf
    , getRowVec
    , getByte
    , vJetHash
    , vJetImpl
    , Exe
    , Jet
    )
where

import Data.Maybe
import Fan.Eval
import Fan.Types
import Hash256
import PlunderPrelude            hiding (traceM, (^))

import Control.Exception (throw)
import Data.Char         (isUpper)
import Data.Text.IO      (hPutStrLn)

import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Vector as V


-- Types -----------------------------------------------------------------------

type Exe = (SmallArray Fan -> Fan)
type Jet = (Exe -> Exe)


-- Globals ---------------------------------------------------------------------

{-
    Very jank hack.  Make sure that the jets-hashes table gets overwritten
    immediatly on startup the first time jet matching happens, we will
    commit to that result forever.
-}
vJetHash :: IORef (Map Text Hash256)
vJetHash = unsafePerformIO (newIORef mempty)

vJetImpl :: IORef (Map Text (Maybe Jet))
vJetImpl = unsafePerformIO (newIORef mempty)


-- Table Construction ----------------------------------------------------------

matchJetsToHash
    :: Map Text Hash256
    -> Map Text (Maybe Jet)
    -> [(Text, Maybe Jet, Hash256)]
matchJetsToHash hashes jets =
    if length hashes == length jets
    then mapMaybe f (mapToList jets)
    else
        let inHashes = S.fromList (M.keys hashes)
            inJets   = S.fromList (M.keys jets)
            jetMissing  = inHashes `S.difference` inJets
            hashMissing = inJets `S.difference` inHashes
        in error $ ("Just hash/impl tables do no match:\n\n" <>)
                 $ ppShow ( ( "Missing from jets table" :: Text
                            , jetMissing
                            )
                          , ( "Missing from hashes table" :: Text
                            , hashMissing
                            )
                          )
  where
    f (t, x) = case lookup t hashes of
        Nothing -> error ("No hash corresponding to jet: " <> unpack t)
        Just hx -> Just (t, x, hx)

table :: [(Text, Maybe Jet, Hash256)]
table = unsafePerformIO do
    matchJetsToHash <$> readIORef vJetHash <*> readIORef vJetImpl

jetsByName :: Map Text (Hash256, Maybe Jet)
jetsByName = mapFromList (table <&> \(n,f,h) -> (n,(h,f)))


-- Jet Matching ----------------------------------------------------------------

spc20 :: Text
spc20 = replicate 20 ' '

pad20 :: Text -> Text -> Text
pad20 pref post = pref <> drop (length pref) spc20 <> post

{-
    This depends on the hash (and therefore the on pin serialization), so
    it's important that this thunk is not invoked until a function is run.

    TODO: This should output using `Debug` (not hPutStrLn.
    TODO: This should maybe not crash if a jet-named function is defined
          that isn't a jet.  Nice for debugging, but bad run-time behavior.
-}
jetMatch :: Pin -> IO Pin
jetMatch cpin = do
    let pinName = case cpin.item of { FUN l -> lawNameText l.name; _ -> "" }

    let envRow :: SmallArray Fan -> Fan
        envRow e = ROW $ arrayFromListN (sizeofSmallArray e) (toList e)

    fallback <- do
        config <- readIORef vRtsConfig
        pure case config.onJetFallback of
                 IGNORE -> cpin.exec
                 CRASH  -> \e -> throw (PRIMOP_CRASH "deopt" (envRow e))
                 WARN   -> \e -> unsafePerformIO do
                                       shw <- readIORef vShowFan
                                       hPutStrLn stderr ("deopt:" <> shw (envRow e))
                                       pure (cpin.exec e)

    let pHash      = cpin.hash
    let hashText   = hashToBTC pHash
    let jetLike    = case toList pinName of '_':c:_ -> isUpper c; _ -> False
    let notMatched = do
                config <- readIORef vRtsConfig
                let doWarn = do
                        hPutStrLn stderr (pad20 pinName "NOT MATCHED")
                        dumpHashLine pinName hashText
                case config.onJetMismatch of
                    IGNORE -> pure ()
                    WARN   -> doWarn
                    CRASH  -> doWarn >> error "Crashing from Jet Mismatch"

    case lookup pinName jetsByName of
        Nothing -> (when jetLike do notMatched) $> cpin
        Just (jetHash, exe) -> do
            if jetHash == pHash then do
                hPutStrLn stderr (pad20 pinName "MATCHED")
                dumpHashLine pinName hashText
                pure case exe of
                         Nothing -> cpin
                         Just ex -> setExec (\env -> ex fallback env) cpin
            else do
                notMatched
                if False then
                    -- TODO Hax XXX Delete This NOW!  This isn't safe at all!
                    -- TODO Hax XXX What are you doing?  Why?  Stop!
                    pure case exe of
                             Nothing -> cpin
                             Just ex -> setExec (\env -> ex fallback env) cpin
                else
                    pure $ cpin

dumpHashLine :: Text -> Text -> IO ()
dumpHashLine pinName hashText = do
    hPutStrLn stderr $ concat
        [ "    , e \"", pinName, "\""
        , pack (replicate (max 1 (20 - length pinName))  ' ')
        , "\"", hashText, "\""
        ]


-- Utils -----------------------------------------------------------------------

getPin :: Fan -> Maybe Pin
getPin (PIN p) = Just p
getPin _       = Nothing

getRow :: Fan -> Maybe (Array Fan)
getRow (ROW xs) = Just xs
getRow _        = Nothing

getRowOf :: (Fan -> Maybe a) -> Fan -> Maybe (Array a)
getRowOf f (ROW xs) = traverse f xs
getRowOf _ _        = Nothing

{-# INLINE getRowVec #-}
getRowVec :: Fan -> Maybe (Vector Fan)
getRowVec = fmap V.fromArray . getRow

getByte :: Fan -> Maybe Word8
getByte (NAT n) | n<256 = Just (fromIntegral n)
getByte _               = Nothing
