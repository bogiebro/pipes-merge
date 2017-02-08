{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Pipes.Merge (steal, broadcast) where
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Pipes
import Pipes.Internal

readQ q    = liftBase $ atomically $ readTBQueue q
writeQ q x = liftBase $ atomically $ writeTBQueue q x

wrapper :: MonadBaseControl IO m => TBQueue (Proxy a' a b' b m r) ->
  Proxy a' a b' b m r
wrapper q = M loop where
  loop = readQ q >>= \case
    Request a' fa -> return $ Request a' $ (>> M loop) . fa
    Respond c fc' -> return $ Respond c $ (>> M loop) . fc'
    Pure r        -> return (Pure r)
    M _           -> error "Should never get an M"

throughVar q x f = do
  var <- newEmptyMVar
  writeQ q $ x $ \v -> M $ putMVar var v >> return (Pure undefined)
  f <$> takeMVar var

throughBar q b x f = do
  var <- liftBase newEmptyTMVarIO
  liftBase $ atomically $ writeTBQueue b var `orElse` (writeTBQueue q $ x $ \v-> M $ do
    liftBase $ atomically $ some (readTBQueue b >>= flip putTMVar v)
    return (Pure undefined))
  f <$> (liftBase $ atomically (takeTMVar var))

-- | Run pipes in parallel, buffering their requests for input and output
--   into a queue of the given size. Input is passed to the first available
--   pipe.
steal :: MonadBaseControl IO m => Int -> [Proxy a' a b' b m r] -> Proxy a' a b' b m r
steal c xs = do 
  q <- lift $ liftBase $ newTBQueueIO c
  let loop (Request a' fa) = throughVar q (Request a') fa >>= loop
      loop (Respond b fb') = throughVar q (Respond b) fb' >>= loop
      loop (M m)           = m >>= loop
      loop (Pure r)        = writeQ q (Pure r)
  lift $ mapM_ (fork . loop) xs
  wrapper q

-- | Like `steal`, but input is passed to all pipes. All pipes must receive
--   an input before the next upstream element can be awaited.
broadcast :: MonadBaseControl IO m => Int -> [Proxy a' a b' b m r] -> Proxy a' a b' b m r
broadcast c xs = do
  b <- lift $ liftBase $ newTBQueueIO (length xs)
  q <- lift $ liftBase $ newTBQueueIO c
  let loop (Request a' fa) = throughBar q b (Request a') fa >>= loop
      loop (Respond b fb') = throughVar q (Respond b) fb' >>= loop
      loop (M m)           = m >>= loop
      loop (Pure r)        = writeQ q (Pure r)
  lift $ mapM_ (fork . loop) xs
  wrapper q
