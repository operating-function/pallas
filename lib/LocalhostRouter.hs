-- | This module contains a variety of localhost-related types and functions.
--   This may not necessarily be the ideal long-term structure/location for
--   these.
module LocalhostRouter where

import           PlunderPrelude

import           Codec.Serialise           (DeserialiseFailure, Serialise (..),
                                            deserialise, deserialiseOrFail,
                                            serialise)
import           Codec.Serialise.Decoding  (Decoder)
import           Codec.Serialise.Decoding  as D
import           Codec.Serialise.Encoding  (Encoding)
import           Codec.Serialise.Encoding  as E
import           Control.Monad.Fail        (fail)
import           Crypto.Sign.Ed25519       (PublicKey (..), Signature (..))
import           Data.Either               (fromRight)
import qualified Data.Text                 as T
import           Data.Binary.Get           (getInt64le, runGet)
import           Data.Binary.Put           (putInt64le, runPut)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           Network.Socket            (Socket, SockAddr (..),
                                            hostAddressToTuple)
import           Network.Socket.ByteString (recv, sendAll)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

-- TODO this is an inefficent encoding which attempts to allow for both ip4 &
-- ip6 addresses.
type HostPort = (String, String)

data RouterRequest
  = Connect
  | IdentifyPropose PublicKey
  | IdentifyChallenge PublicKey Signature HostPort
  -- ^ HostPort is needed b/c the machine's machine-machine HostPort would be
  -- different from the one used to communicate w/ the router.
  | Resolve PublicKey
  deriving (Show)

data ChalStatus
  = CsValid
  | CsInvalid
  | CsMissingChal
  deriving (Generic, Show)

data RouterResponse
  = ConnectResp
  | IdentifyProposeResp BS.ByteString
  | IdentifyChallengeResp ChalStatus
  | ResolveResp (Maybe HostPort)
  deriving (Show)

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

encodePk :: PublicKey -> Encoding
encodePk = E.encodeBytes . unPublicKey

decodePk :: Decoder s PublicKey
decodePk = PublicKey <$> D.decodeBytes

encodeSig :: Signature -> Encoding
encodeSig = E.encodeBytes . unSignature

decodeSig :: Decoder s Signature
decodeSig = Signature <$> D.decodeBytes

encodeMaybe :: (a -> Encoding) -> Maybe a -> Encoding
encodeMaybe _   Nothing  = encodeListLen 0
encodeMaybe enc (Just x) = encodeListLen 1 <> enc x

decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe dec = do
  n <- D.decodeListLen
  case n of
    0 -> pure Nothing
    1 -> do !x <- dec
            pure (Just x)
    _ -> fail ("decodeMaybe: invalid tag: " <> show n)

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

instance Serialise ChalStatus

instance Serialise RouterRequest where
  encode Connect = E.encodeTag 0
  encode (IdentifyPropose pk) = mconcat
    [ E.encodeTag 1
    , encodePk pk
    ]
  encode (IdentifyChallenge pk sig hp) = mconcat
    [ E.encodeTag 2
    , encodePk pk
    , encodeSig sig
    , encode hp
    ]
  encode (Resolve pk) = mconcat
    [ E.encodeTag 3
    , encodePk pk
    ]

  decode = do
    t <- D.decodeTag
    case t of
      0 -> pure Connect
      1 -> do
        pk <- decodePk
        pure (IdentifyPropose pk)
      2 -> do
        pk <- decodePk
        sig <- decodeSig
        hp <- decode
        pure (IdentifyChallenge pk sig hp)
      3 -> do
        pk <- decodePk
        pure (Resolve pk)
      _ -> fail ("decode RouterRequest: invalid tag: " <> show t)

instance Serialise RouterResponse where
  encode ConnectResp = E.encodeTag 0
  encode (IdentifyProposeResp chal) = mconcat
    [ E.encodeTag 1
    , encode chal
    ]
  encode (IdentifyChallengeResp status) = mconcat
    [ E.encodeTag 2
    , encode status
    ]
  encode (ResolveResp mb_sa) = mconcat
    [ E.encodeTag 3
    , encode mb_sa
    ]

  decode = do
    t <- D.decodeTag
    case t of
      0 -> pure ConnectResp
      1 -> do
        chal <- decode
        pure (IdentifyProposeResp chal)
      2 -> do
        status <- decode
        pure (IdentifyChallengeResp status)
      3 -> do
        mb_sa <- decode
        pure (ResolveResp mb_sa)

sockAddrToHostPort :: SockAddr -> HostPort
sockAddrToHostPort (SockAddrInet port host) = (renderedHost, show port)
 where
  (a,b,c,d) = hostAddressToTuple host
  renderedHost = intercalate "." (show <$> [a,b,c,d])
sockAddrToHostPort (SockAddrInet6 _port _ _host _) = error "bad: no ip6"
sockAddrToHostPort (SockAddrUnix _) = error "bad: SockAddrUnix"

-- packet format:
--   - header: 8 byte Int, little-endian, encoding the length of the payload
--   - payload
-- TODO use CBOR incremental decoding interface to avoid having an Int64 header,
-- even when the `len` is small?

sendSer :: (Serialise a, Show a) => Socket -> a -> IO ()
sendSer s x = sendAll s (BL.toStrict (header <> payload))
 where
  payload = serialise x
  header  = runPut (putInt64le (fromIntegral (BL.length payload)))

-- TODO do something about `Int` / `Int64` conversion mismatch on 32-bit
-- systems?

recvDeser :: (Serialise a, Show a) => Socket -> IO (Either DeserialiseFailure a)
recvDeser sock = do
  chunk <- lazyRecv
  let (headerBytes, payloadBytes) = BL.splitAt 8 chunk
      len = fromIntegral (runGet getInt64le headerBytes)
  if len + 8 < chunkSize
     then pure (deserialiseOrFail payloadBytes)
     else do
       let numChunksLeft = divExactOrRoundUp (len + 8 - chunkSize) chunkSize
       bss <- replicateM numChunksLeft lazyRecv
       pure (deserialiseOrFail (mconcat (payloadBytes : bss)))
 where
  lazyRecv = BL.fromStrict <$> recv sock chunkSize
  chunkSize = 4096

divExactOrRoundUp x y =
  let (q,r) = x `quotRem` y
   in if r == 0
         then q
         else q + 1

recvDeserInfallible :: (Serialise a, Show a) => Socket -> IO a
recvDeserInfallible sock = fromRight (error "impossible") <$> recvDeser sock
