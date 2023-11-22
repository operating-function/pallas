-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Block
    ( blocksFile
    , Block
    , replFile
    , replHandle
    , replStdin
    , replText
    , parseBlocks
    )
where

import PlunderPrelude

import Control.Monad.Catch (MonadThrow)
import Rex.Policy          (Block(..))
import Rex.Print           (blocksFile)
import Rex.Types           (Rex)

import qualified Rex.Mechanism            as Rex
import qualified Rex.Policy               as Rex
import qualified Data.ByteString          as BS

-- Convert a stream of lines into a stream of blocks ---------------------------

-- rexStep :: BlockState -> Maybe ByteString -> (BlockState, [Either Text (Int, Rex)])
-- rexStep st mInp = fmap (sequence . fmap treesRexChecked) <$> treeStep st mInp

replHandle :: (MonadIO m, MonadThrow m)
           => FilePath -> Handle -> (Block -> m ())
           -> m ()
replHandle pax h act =
    go (Rex.blockState pax)
  where
    go st1 = do
        mBar <- liftIO (try $ BS.hGetLine h) >>= \case
                  Left (e :: IOError) | isEOFError e -> pure Nothing
                  Left (e :: IOError)                -> throwIO e
                  Right ln                           -> pure (Just ln)
        let (st2, outputs) = Rex.rexStep st1 mBar
        traverse_ act outputs -- for_ outputs (liftIO . act)
        unless (mBar == Nothing) do go st2

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
    go (Rex.blockState fil) [] (encodeUtf8 <$> lines tex)
  where
    go st1 acc1 moreLines =
        let
            (st2, outputs) = Rex.rexStep st1 (headMay moreLines)
            acc2 = acc1 <> fmap act outputs
        in
            case moreLines of
                []   -> intercalate "\n" acc2
                _:ls -> go st2 acc2 ls

parseBlocks :: FilePath -> Text -> Either Text [Rex]
parseBlocks fil tex =
    go (Rex.blockState fil) [] (encodeUtf8 <$> lines tex)
  where
    go st1 acc1 moreLines = do
        let (st2, outputs) = Rex.rexStep st1 (headMay moreLines)
        newRexes <- for outputs \b -> case b.errors of
                                          e:_ -> Left e
                                          []  -> pure b.rex
        let acc2 = acc1 <> newRexes
        case moreLines of
            []   -> pure acc2
            _:ls -> go st2 acc2 ls
