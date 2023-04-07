{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Block
    ( blocksFile
    , Block(..)
    , linesBlocks
    , replFile
    , replHandle
    , replStdin
    , replText
    , parseBlocks
    )
where

import PlunderPrelude

import Control.Monad.Catch (MonadThrow)
import Data.Conduit        ((.|))
import Rex.Lexer           (isNameChar, isRuneChar, lexLine)
import Rex.Parser          (parseBlock)
import Rex.Print           (blocksFile)
import Rex.Types           (Rex)

import qualified Data.Conduit             as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List        as C
import qualified Data.Text                as T

-- Convert a stream of lines into a stream of blocks ---------------------------

data Block = BLK
    { lineOff :: Int
    , input   :: Text
    , eRex    :: Either Text Rex
    }

replHandle :: (MonadIO m, MonadThrow m)
           => FilePath -> Handle -> (Block -> m ())
           -> m ()
replHandle pax h act =
  C.runConduit $ C.sourceHandle h
              .| C.decodeUtf8
              .| C.linesUnbounded
              .| linesBlocks
              .| blockRex pax
              .| consumed
 where
  consumed = C.await >>= \case
    Nothing -> pure ()
    Just rb -> lift (act rb) >> consumed

replFile :: (MonadIO m, MonadThrow m)
         => FilePath -> (Block -> m ())
         -> m ()
replFile pax act = do
  h <- liftIO (openFile pax ReadMode)
  replHandle pax h act

replStdin :: (Block -> IO ()) -> IO ()
replStdin act = replHandle "stdin" stdin act

replText :: FilePath -> Text -> (Block -> Text) -> Text
replText fil tex act =
  C.runConduitPure $ C.sourceList (lines tex)
                  .| linesBlocks
                  .| blockRex fil
                  .| consumed []
 where
  consumed acc = C.await >>= \case
    Nothing -> pure $ intercalate "\n" $ reverse acc
    Just rb -> consumed (act rb : acc)

--- Note that `parseBlock` will only return `Nothing` when it is given
--- a sequence of black lines at the end of a file.
blockRex :: Monad m => FilePath -> C.ConduitT [Text] Block m ()
blockRex pax = loop 0
  where
    loop lin = do
        mLns <- C.await
        whenJust mLns \lns -> do
            let lxLn = \(l,n) -> lexLine (pax,n) l
            let toks = traverse lxLn (zip lns [lin..])
            let mRex = sequence (toks >>= parseBlock)
            whenJust mRex $ \rex ->
                C.yield $ BLK lin (T.unlines lns) rex
            loop (lin + length lns)

linesBlocks :: Monad m => C.ConduitT Text [Text] m ()
linesBlocks = loop False []
  where
    out [] = pure ()
    out ln = C.yield (reverse ln)

    loop seenAny blk =
        C.await >>= \case
            Nothing ->
                out blk >> pure ()
            Just (T.stripEnd -> ln) ->
                if | not seenAny && nullLn ln -> loop False (ln:blk)
                   | lineEndsBlock seenAny ln -> out (ln:blk) >> loop False []
                   | otherwise                -> loop True (ln:blk)

nullLn :: Text -> Bool
nullLn "" = True
nullLn t  = (";" `isPrefixOf` T.stripStart t)

lineEndsBlock :: Bool -> Text -> Bool
lineEndsBlock seenAny ln =
    case unpack ln of
        ' ':_                  -> False -- Indented block
        ';':_                  -> False -- Comment (could be multi-line)
        '"':'"':'"':_          -> False -- Page (could be multi-line)
        '\'':'\'':'\'':_       -> False -- Page (could be multi-line)
        c:cs | isOpenRune c cs -> False -- Continuation rune
        c:cs | isNamePage c cs -> False
        []                     -> True  -- Empty line
        _                      -> not seenAny  -- One line
  where
    isNamePage :: Char -> String -> Bool
    isNamePage '"'  ('"':'"':_)   = True
    isNamePage '\'' ('\'':'\'':_) = True
    isNamePage _    []            = False
    isNamePage c    (d:dd)        = isNameChar c && isNamePage d dd

    isOpenRune :: Char -> String -> Bool
    isOpenRune c cs = isRuneChar c && isOpenRuneTail cs

    isOpenRuneTail :: String -> Bool
    isOpenRuneTail []      = True
    isOpenRuneTail (' ':_) = True
    isOpenRuneTail (c:cs)  = isRuneChar c && isOpenRuneTail cs

parseBlocks :: FilePath -> Text -> Either Text [Rex]
parseBlocks fil tex
    = consume
    $ C.runConduitPure
    $ C.sourceList (lines tex)
    .| linesBlocks
    .| blockRex fil
    .| C.sinkList
  where
    consume :: [Block] -> Either Text [Rex]
    consume = sequence . fmap (.eRex)
