module Server.Util where

import PlunderPrelude

import Control.Monad.State (MonadState, StateT, evalStateT, execState, get,
                            gets, modify', put, runStateT)

import Optics
import Optics.At

import qualified Data.Map      as M
import qualified Data.Sequence as Q
import qualified Data.Set      as S

appendSeq :: a -> Seq a -> Seq a
appendSeq a s = s Q.|> a

deleteFirst :: Eq a => a -> Seq a -> Seq a
deleteFirst a s = case Q.findIndexL (== a) s of
  Nothing -> s
  Just i  -> Q.deleteAt i s

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM fun []     = pure True
allM fun (x:xs) = fun x >>= \case
  True  -> allM fun xs
  False -> pure False

setHead :: Set a -> Maybe a
setHead s = case S.null s of
  True  -> Nothing
  False -> listToMaybe $ S.toAscList s

-- -----------------------------------------------------------------------

data IntFreelist = IntFreelist {
  intFreelistFreelist :: Seq Int,
  intFreelistNext     :: Int
  }

makeFieldLabels ''IntFreelist

emptyIntFreelist = IntFreelist Q.empty 0

freelistGetNext :: Monad m => StateT IntFreelist m Int
freelistGetNext = do
  freelist <- use #freelist
  case freelist of
    Q.Empty -> do
      -- No holes to fill in the Snapshot array.
      num <- use #next
      modifying #next (+1)
      pure num
    (x Q.:<| xs) -> do
      assign #freelist xs
      pure x

freelistPush :: Monad m => Int -> StateT IntFreelist m ()
freelistPush i = modifying' #freelist (appendSeq i)

freelistFilter :: Monad m => Int -> StateT IntFreelist m ()
freelistFilter i = modifying' #freelist (filter (/= i))

-- Like stateTVar, but takes monadic state.
stateTTVar :: TVar s -> StateT s STM a -> STM a
stateTTVar var f = do
   s <- readTVar var
   (a, s') <- runStateT f s
   writeTVar var s'
   return a


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _    = pure ()
whenJust (Just x) fun = fun x


{- moveItem #srcMap #dstMap key -}
moveItem :: (MonadState s m,
             Is k1 A_Setter,
             Is k2 A_Setter,
             Is k2 A_Getter,
             Ord k3)
         => Optic' k2 is1 s (Map k3 a)
         -> Optic k1 is2 s s (Map k3 a) (Map k3 a)
         -> k3
         -> m ()
moveItem src dst key = do
  use src <&> lookup key >>= \case
    Nothing -> pure ()
    Just value -> do
      modifying' src (M.delete key)
      modifying' dst (M.insert key value)
