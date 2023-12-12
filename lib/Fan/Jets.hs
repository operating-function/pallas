-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Fan.Jets
    ( vTrkFan
    , vShowFan
    , jetMatch
    , getRow
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
import Data.Text.IO      (hPutStr, hPutStrLn)

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

vJetImpl :: IORef (Map Text Jet)
vJetImpl = unsafePerformIO (newIORef mempty)


-- Table Construction ----------------------------------------------------------

matchJetsToHash
    :: Map Text Hash256
    -> [(Text, Jet)]
    -> [(Text, Jet, Hash256)]
matchJetsToHash tab =
    mapMaybe \(t,x) -> do
        bs <- case lookup t tab of
                  Just r  -> Just r
                  Nothing -> do
                      _ <- error ("No hash corresponding to jet: " <> unpack t)
                      Nothing
        pure (t,x,bs)

table :: [(Text, Jet, Hash256)]
table = unsafePerformIO do
    matchJetsToHash <$> readIORef vJetHash
                    <*> (mapToList <$> readIORef vJetImpl)

jetsByName :: Map Text (Hash256, Jet)
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
        doCrash <- readIORef vCrashOnJetFallback
        doWarn  <- readIORef vWarnOnJetFallback
        pure case (doCrash, doWarn) of
                 (True, _)      -> \e -> throw (PRIMOP_CRASH "deopt" (envRow e))
                 (_, True)      -> \e -> unsafePerformIO do
                                       shw <- readIORef vShowFan
                                       putStrLn ("deopt:" <> shw (envRow e))
                                       pure (cpin.exec e)
                 (False, False) -> cpin.exec

    let pHash      = cpin.hash
    let hashText   = hashToBTC pHash
    let jetLike    = case toList pinName of '_':c:_ -> isUpper c; _ -> False
    let notMatched = do
                hPutStrLn stderr (pad20 pinName "NOT MATCHED")
                dumpHashLine pinName hashText

    case lookup pinName jetsByName of
        Nothing -> (when jetLike do notMatched) $> cpin
        Just (jetHash, exe) -> do
            if jetHash == pHash then do
                hPutStrLn stderr (pad20 pinName "MATCHED")
                dumpHashLine pinName hashText
                pure $ setExec (\env -> exe fallback env) cpin
            else do
                notMatched
                if False then
                    -- TODO Hax XXX Delete This NOW!  This isn't safe at all!
                    -- What are you doing?  Why?  Stop!
                    pure $ setExec (\env -> exe fallback env) cpin
                else
                    pure $ cpin

dumpHashLine :: Text -> Text -> IO ()
dumpHashLine pinName hashText = do
    hPutStr stderr "    , e \""
    hPutStr stderr pinName
    hPutStr stderr "\""
    replicateM_ (max 1 (20 - length pinName)) do
        hPutStr stderr " "
    hPutStr stderr "\""
    hPutStr stderr hashText
    hPutStr stderr "\"\n"


-- Utils -----------------------------------------------------------------------

getRow :: Fan -> Maybe (Array Fan)
getRow (ROW xs) = Just xs
getRow _        = Nothing

{-# INLINE getRowVec #-}
getRowVec :: Fan -> Maybe (Vector Fan)
getRowVec = fmap V.fromArray . getRow

getByte :: Fan -> Maybe Word8
getByte (NAT n) | n<256 = Just (fromIntegral n)
getByte _               = Nothing
