-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall         #-}
{-# OPTIONS_GHC -Werror       #-}

module ChatMock (main) where

import ClassyPrelude
import Nat
import Optics

import ChatMock.Hub.Packet   ()
import ChatMock.Hub.Protocol ()

import Crypto.Sign.Ed25519     (PublicKey(..), SecretKey, createKeypair)
import Data.Aeson              (FromJSON, ToJSON)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Text.IO            (hPutStrLn)
import Data.Time.Clock         (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime)
import System.Posix.Directory  (getWorkingDirectory)

import qualified Data.Aeson                     as A
import qualified Network.Wai.Application.Static as WSt
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as W
import qualified Network.WebSockets             as WS


-- Types -----------------------------------------------------------------------

newtype Addr = ADDR { hex :: Text }  -- Hex Encoded Word256
  deriving (Generic)
  deriving newtype (FromJSON, ToJSON, IsString, Eq, Ord)

instance Show Addr where
    show addr = show (take 8 addr.hex)

newtype Name = NAME { text :: Text } -- NonEmpty Alpha-Numeric String
  deriving newtype (ToJSON, FromJSON, IsString, Eq, Ord, Show)

data Message = MSG                   -- Message
    { ours :: Bool
    , when :: POSIXTime
    , body :: Text
    }
  deriving (Eq, Ord, Show)

data Contact = CONTACT
    { nickname :: Name
    , draft    :: Text
    , mailbox  :: [Message]
    }
  deriving (Eq, Ord, Show)

{-
    The name of an identity is the name of it's self-contact.  Notes are
    self messages.  All self-messages are `ours`.  Self messages are
    not actually sent, so we never actually receive a self-message.
-}
data Account = ACCT
    { publicKey :: !PublicKey
    , secretKey :: !SecretKey
    , contacts  :: !(Map Addr Contact)
    }
  deriving (Eq)

type AppState = Map Addr Account

data UserCmd
    = UC_NewAccount     Name
    | UC_RemoveAccount  Addr
    | UC_NewContact     Addr Addr
    | UC_RemoveContact  Addr Addr
    | UC_NameContact    Addr Addr Name
    | UC_SaveDraft      Addr Addr Text
    | UC_SendMessage    Addr Addr Text
    | UC_ClearHistory   Addr Addr
  deriving (Generic, Show)

data NetCmd = NC_TakeMessage Addr Addr Text
  deriving (Generic, Show)

data Event
    = E_AccountGenerated Addr PublicKey SecretKey
    | E_Web WebEvent
  deriving (Show)

data WebEvent
    = E_AccountAdded      Addr
    | E_AccountRemoved    Addr
    | E_ContactAdded      Addr Addr
    | E_ContactRemoved    Addr Addr
    | E_ContactNamed      Addr Addr Name
    | E_DraftSet          Addr Addr Text
    | E_MessageSent       Addr Addr Text POSIXTime
    | E_MessageReceived   Addr Addr Text POSIXTime
    | E_HistoryCleared    Addr Addr
    | E_LoadingComplete
  deriving (Generic, Show)

type Cmd = Either UserCmd NetCmd

data State = STATE
    { appSt :: TVar AppState
    , input :: TQueue Cmd
    , subs  :: TVar [TVar (Maybe (TQueue Event))]
    }


-- Lenses ----------------------------------------------------------------------

makeFieldLabelsNoPrefix ''Contact
makeFieldLabelsNoPrefix ''Account


-- JSON Encoding and Decoding --------------------------------------------------

instance FromJSON UserCmd where
    parseJSON = A.genericParseJSON
               $ A.defaultOptions { A.constructorTagModifier = drop 3 }

instance ToJSON UserCmd where
    toEncoding = A.genericToEncoding
               $ A.defaultOptions { A.constructorTagModifier = drop 3 }

instance FromJSON WebEvent where
    parseJSON = A.genericParseJSON
               $ A.defaultOptions { A.constructorTagModifier = drop 2 }

instance ToJSON WebEvent where
    toEncoding = A.genericToEncoding
               $ A.defaultOptions { A.constructorTagModifier = drop 2 }

eventToJSON :: WebEvent -> ByteString
eventToJSON = toStrict . A.encode


-- State Transitions -----------------------------------------------------------

blankContact :: Contact
blankContact = CONTACT "" "" []

applyEv :: AppState -> Event -> AppState
applyEv st = \case
    E_AccountGenerated us pub priv ->
        over (at us) (Just . fromMaybe (ACCT pub priv mempty)) st
    E_Web (E_AccountAdded _us) ->
        st -- Always bundled with E_AccountGenerated
    E_Web (E_AccountRemoved us) ->
        deleteMap us st
    E_Web (E_ContactAdded us them) ->
        let
            pax = (at us % _Just % #contacts % at them)
        in
            over pax (Just . fromMaybe blankContact) st
    E_Web (E_ContactRemoved us them) ->
        let
            pax = (at us % _Just % #contacts % at them)
        in
            over pax (const Nothing) st
    E_Web (E_ContactNamed us them name) ->
        let
            pax = (at us % _Just % #contacts % at them % _Just % #nickname)
        in
            set pax name st
    E_Web (E_DraftSet us them body) ->
        let
            pax = (at us % _Just % #contacts % at them % _Just % #draft)
        in
            set pax body st
    E_Web (E_MessageSent us them body wen) ->
        let
            msg = MSG True wen body
            pax = (at us % _Just % #contacts % at them % _Just % #mailbox)
        in
            over pax (msg:) st
    E_Web (E_MessageReceived us them body wen) ->
        let
            msg = MSG False wen body
            pax = (at us % _Just % #contacts % at them % _Just % #mailbox)
        in
            over pax (msg:) st
    E_Web (E_HistoryCleared us them) ->
        let
            pax = (at us % _Just % #contacts % at them % _Just % #mailbox)
        in
            set pax [] st
    E_Web (E_LoadingComplete) ->
        st

generateAddress :: IO (Addr, PublicKey, SecretKey)
generateAddress = do
    (pub, priv) <- createKeypair
    pure (pubToAddr pub, pub, priv)
  where
    pubToAddr = ADDR
              . decodeUtf8
              . toStrict
              . toLazyByteString
              . byteStringHex
              . unPublicKey

executeCommand :: Cmd -> IO [Event]
executeCommand = \case
    Left (UC_NewAccount name) -> do
        (us, pub, priv) <- generateAddress
        pure [ E_Web $ E_AccountAdded us
             , E_AccountGenerated us pub priv
             , E_Web $ E_ContactAdded us us
             , E_Web $ E_ContactNamed us us name
             ]
    Left (UC_RemoveAccount us) -> do
        pure [E_Web $ E_AccountRemoved us]
    Left (UC_NewContact us them) -> do
        pure [E_Web $ E_ContactAdded us them]
    Left (UC_RemoveContact us them) -> do
        pure [E_Web $ E_ContactRemoved us them]
    Left (UC_NameContact us them name) -> do
        pure [E_Web $ E_ContactNamed us them name]
    Left (UC_SaveDraft us them body) -> do
        pure [E_Web $ E_DraftSet us them body]
    Left (UC_SendMessage us them body) -> do
        wen <- getPOSIXTime
        pure [E_Web $ E_MessageSent us them body wen]
    Left (UC_ClearHistory us them) -> do
        pure [E_Web $ E_HistoryCleared us them]
    Right (NC_TakeMessage us them body) -> do
        wen <- getPOSIXTime
        pure [E_Web $ E_MessageReceived us them body wen]

reconstructState :: AppState -> [Event]
reconstructState st =
    concat (uncurry reconstructAccount <$> mapToList st)

reconstructAccount :: Addr -> Account -> [Event]
reconstructAccount addr account =
    concat
        [ [ E_AccountGenerated addr account.publicKey account.secretKey
          , E_Web (E_AccountAdded addr)
          ]
        , concat $ fmap (uncurry $ reconstructContact addr)
                 $ mapToList account.contacts
        , [E_Web E_LoadingComplete]
        ]

reconstructContact :: Addr -> Addr -> Contact -> [Event]
reconstructContact us them x =
    concat
        [ [E_Web (E_ContactAdded us them)]
        , if null x.draft then [] else
            [E_Web (E_DraftSet us them x.draft)]
        , if null x.nickname.text then [] else
            [E_Web (E_ContactNamed us them x.nickname)]
        , reconstructMessage us them <$> reverse x.mailbox
        ]

reconstructMessage :: Addr -> Addr -> Message -> Event
reconstructMessage us them mes =
    if mes.ours then
        E_Web $ E_MessageSent us them mes.body mes.when
    else
        E_Web $ E_MessageReceived us them mes.body mes.when

-- Values ----------------------------------------------------------------------


{-
    This hackily relies on the fact that a E_ReceivedMessage has no
    effect if the account doesn't exist.  It would be better to just not
    emit the event in that case, and that's eventually what will happen
    once this is actually hooked up to the network.
-}
doSend :: State -> UserCmd -> IO ()
doSend st = \case
    UC_SendMessage us them body ->
        atomically $ writeTQueue st.input
                   $ Right
                   $ NC_TakeMessage them us body
    _ ->
        pure ()

pure []

newState :: AppState -> STM State
newState as = do
    appSt <- newTVar as
    input <- newTQueue
    subs  <- newTVar []
    pure STATE{..}

step :: AppState -> Cmd -> IO (AppState, [Event])
step st cmd = do
    evs <- executeCommand cmd
    pure (foldl' applyEv st evs, evs)

broadcast :: State -> [Event] -> STM ()
broadcast st evs = do
    old <- readTVar st.subs
    new <- go [] old
    writeTVar st.subs new
  where
    go :: [TVar (Maybe (TQueue Event))]
       -> [TVar (Maybe (TQueue Event))]
       -> STM [TVar (Maybe (TQueue Event))]
    go acc []     = pure acc
    go acc (s:ss) = do
        readTVar s >>= \case
            Nothing -> go acc ss
            Just sv -> do
                for_ evs (writeTQueue sv)
                go (s:acc) ss

stateThread :: State -> IO (Async ())
stateThread st = async $ forever do
    (cmd, old) <- atomically ((,) <$> readTQueue st.input <*> readTVar st.appSt)
    print ("StTh got input"::Text, cmd)
    (new, evs) <- step old cmd
    case cmd of
        Left uc -> doSend st uc
        Right{} -> pure ()
    putStrLn "Update + Broadcast"
    atomically do
        writeTVar st.appSt new
        broadcast st evs
    putStrLn "Waiting for more input"

wsApp :: State -> WS.PendingConnection -> IO ()
wsApp st conn = do
    sock <- WS.acceptRequest conn
    let logPing = hPutStrLn stderr "WEBSOCKET: ping"
    (initialState, chan, subscription) <-
        atomically do
            ass <- readTVar st.appSt
            chn <- newTQueue
            sub <- newTVar (Just chn)
            modifyTVar st.subs (sub:)
            pure (ass, chn, sub)

    subVals <- atomically do
        subs <- readTVar st.subs
        fmap (fmap $ pure()) <$> traverse readTVar subs

    print ("num subs"::Text, length subVals)
    print ("subs"::Text, subVals)

    WS.withPingThread sock 5 logPing do
        for_ (reconstructState initialState) \ev ->
            case ev of
                E_Web webEv -> WS.sendBinaryData sock (eventToJSON webEv)
                _           -> pure ()

        evThread <- async $ forever do
            ev <- atomically (readTQueue chan)
            print ("Queue got event"::Text, ev)
            case ev of
                E_Web webEv -> WS.sendBinaryData sock $ eventToJSON webEv
                _           -> pure ()

        _ <- async do
            void (waitCatch evThread)
            putStrLn "IT DIE!!"
            atomically (writeTVar subscription Nothing)

        forever do
            buf <- try (WS.receiveData @ByteString sock) >>= \case
                       Left (e :: SomeException) -> do
                           print ("DIED"::Text, e)
                           cancel evThread
                           throwIO e
                       Right bs -> pure bs
            case A.decodeStrict buf of
                Nothing -> putStrLn "BAD JSON"
                Just v  -> print (v :: A.Value)
            case A.decodeStrict buf of
                Nothing -> do
                    print ("Bad Command"::Text, buf)
                Just xyz -> do
                    print ("Good Command"::Text, xyz)
                    atomically (writeTQueue st.input (Left xyz))




-- Main Loop -------------------------------------------------------------------

pure []

runServer :: AppState -> IO ()
runServer initialSt = do
    let initialEvs = reconstructState initialSt
    for_ initialEvs \case
        E_Web ev -> (putStrLn . decodeUtf8 . toStrict . A.encode) ev
        otherEv  -> print otherEv
    let st' = foldl' applyEv mempty initialEvs

    unless (st' == initialSt) do
        error "Internal Error: Failed to reconstruct initial state"

    (print . A.encode)
        $ (UC_NameContact (ADDR "ff00xx") (ADDR "ff00xx") "Alice")

    vas <- atomically (newState initialSt)
    dir <- getWorkingDirectory
    let opt = WSt.defaultWebAppSettings dir

    _tid <- stateThread vas

    W.run 8888
        $ W.websocketsOr WS.defaultConnectionOptions (wsApp vas)
        $ WSt.staticApp opt


-- Mock Data for Testing -------------------------------------------------------

pure []

mkContact :: Name -> Text -> [Message] -> Contact
mkContact nick draft mail = CONTACT
    { nickname = nick
    , draft = draft
    , mailbox = mail
    }

mkToyState :: IO AppState
mkToyState = do
  (aliceAddr, alicePublic, aliceSecret) <- generateAddress
  (robbyAddr, robbyPublic, robbySecret) <- generateAddress
  (charlAddr, charlPublic, charlSecret) <- generateAddress
  pure $ mapFromList
    [ ( aliceAddr
      , ACCT
        { publicKey = alicePublic
        , secretKey = aliceSecret
        , contacts = mapFromList
          [ (aliceAddr, mkContact "Alice" "I'm not sure what I think about these..." aliceNotes )
          , (robbyAddr, mkContact "Bobby" "Um..." [] )
          , (charlAddr, mkContact "Charlie" "Soo..." [])
          ]
        }
      )
    , ( robbyAddr
      , ACCT
        { publicKey = robbyPublic
        , secretKey = robbySecret
        , contacts = mapFromList
            [ (robbyAddr, mkContact "Robert" "" bobNotes)
            , (aliceAddr, mkContact "Ali" "" [])
            , (charlAddr, mkContact "Charlie" "" bobMailboxFromCharlie)
            ]
        }
      )
    , ( charlAddr
      , ACCT
        { publicKey = charlPublic
        , secretKey = charlSecret
        , contacts = mapFromList
           [ (charlAddr, mkContact "Charles" "" charlieNotes)
           , (aliceAddr, mkContact "Ali" "" [])
           , (robbyAddr, mkContact "Robert" "" charlieMailboxFromBob)
           ]
        }
      )
    ]

epoch :: Natural -> POSIXTime
epoch = secondsToNominalDiffTime . fromIntegral

aliceNotes :: [Message]
aliceNotes = reverse
    [ MSG {ours=True, when=epoch 1000333, body="Hi Alice!"}
    , MSG {ours=True, when=epoch 3000333, body="Hi back Alice!"}
    , MSG {ours=True, when=epoch 5000333, body="brb"}
    ]

bobNotes :: [Message]
bobNotes = reverse
    [ MSG {ours=True, when=epoch 1000333, body="Hi Robert!"}
    , MSG {ours=True, when=epoch 3000333, body="Hi back Robert!"}
    , MSG {ours=True, when=epoch 5000333, body="brb"}
    ]

charlieNotes :: [Message]
charlieNotes = reverse
    [ MSG{ours=True, when=epoch 1000333, body="Hi Charles!"}
    , MSG{ours=True, when=epoch 3000333, body="Hi back Charles!"}
    , MSG{ours=True, when=epoch 5000333, body="brb"}
    ]

bobMailboxFromCharlie :: [Message]
bobMailboxFromCharlie = flipDirection <$> charlieMailboxFromBob

flipDirection :: Message -> Message
flipDirection msg = msg { ours = not msg.ours }

charlieMailboxFromBob :: [Message]
charlieMailboxFromBob = reverse
    [ MSG {ours=False, when=epoch 1000333, body="Hi Charles!"}
    , MSG {ours=True, when=epoch 3000333, body="Hi back Robert!"}
    , MSG {ours=False, when=epoch 5000333, body="brb"}
    ]


-- Program Entry Point ---------------------------------------------------------

main :: IO ()
main = mkToyState >>= runServer
