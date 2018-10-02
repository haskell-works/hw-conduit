module HaskellWorks.Data.Conduit.Combinator where

import Control.Concurrent        (MVar, putMVar, tryTakeMVar)
import Control.Monad             (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Data.Conduit
import Data.Maybe
import Data.Time.Clock.POSIX     as T

import qualified Data.Conduit.List as L

-- | Run the provided conduit in the Just case
maybeC :: Monad m => ConduitT () () m () -> ConduitT a c m () -> ConduitT (Maybe a) (Maybe c) m ()
maybeC n j = getZipConduit
  $   ZipConduit (L.filter isNothing  .| L.map (const ()) .|  n .| L.map (const Nothing))
  <*  ZipConduit (L.concat            .|                      j .| L.map Just           )

-- | Run the provided conduit in the Just case
justC :: Monad m => ConduitT a c m () -> ConduitT (Maybe a) (Maybe c) m ()
justC = maybeC (L.map id)

-- | Run the provided conduit in the Just case
nothingC :: Monad m => ConduitT () () m () -> ConduitT (Maybe a) (Maybe a) m ()
nothingC n = maybeC n (L.map id)

-- | Run the provided conduits on the left and right side of the either respectively
eitherC :: Monad m => ConduitT l a m () -> ConduitT r a m () -> ConduitT (Either l r) a m ()
eitherC l r = getZipConduit
  $   ZipConduit (projectLefts  .| l)
  <*  ZipConduit (projectRights .| r)

-- | Run the conduit on the right side of the either
rightC :: Monad m => ConduitT r a m () -> ConduitT (Either l r) (Either l a) m ()
rightC r = eitherC (L.map Left) (r .| L.map Right)

-- | Run the conduit on the left side of the either
leftC :: Monad m => ConduitT l a m () -> ConduitT (Either l r) (Either a r) m ()
leftC l = eitherC (l .| L.map Left) (L.map Right)

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effectC :: Monad m => (a -> m b) -> ConduitT a a m ()
effectC f = L.mapM (\a -> f a >> return a)

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effectC' :: Monad m => m b -> ConduitT a a m ()
effectC' m = L.mapM (\a -> m >> return a)

-- | Sink that writes the message to an mvar
mvarWriteC :: MonadIO m => MVar a -> ConduitT a Void m ()
mvarWriteC mvar = awaitForever $ \v ->
  liftIO $ tryTakeMVar mvar >> putMVar mvar v

-- | Sink that writes the message to an mvar
mvarWriteMC :: MonadIO m => (a -> b) -> MVar b -> ConduitT a Void m ()
mvarWriteMC f mvar = awaitForever $ \v ->
  liftIO $ tryTakeMVar mvar >> putMVar mvar (f v)

-- | Sink that writes the message to an mvar
mvarWriteSink :: MonadIO m => MVar a -> ConduitT a Void m ()
mvarWriteSink mvar = awaitForever $ \v ->
  liftIO $ tryTakeMVar mvar >> putMVar mvar v

-- | Creates a unified sink, which is actually two separate sinks with results
-- being sent to one or the other based on a predicate.
sinkWithPred :: Monad m => (a -> Bool) -> ConduitT a Void m () -> ConduitT a Void m () -> ConduitT a Void m ()
sinkWithPred p tr fl =
  void $ sequenceSinks [L.filter p .| tr, L.filter (not . p) .| fl]
{-# INLINE sinkWithPred #-}

-- | Projects nothings from the stream.
-- Returns a stream that only contains nothings (represented by unit)
projectNothings :: Monad m => ConduitT (Maybe a) () m ()
projectNothings = awaitForever $ maybe (yield ()) (const $ return ())
{-# INLINE projectNothings #-}

-- | Projects left side values for each value in a stream.
-- Downstream only receives values that were on the left side,
-- the right side is ignored.
projectLefts :: Monad m => ConduitT (Either l r) l m ()
projectLefts = awaitForever $ either yield (const $ return ())
{-# INLINE projectLefts #-}

-- | Projects right side values for each value in a stream.
-- Downstream only receives values that were on the right side,
-- the left side is ignored.
projectRights :: Monad m => ConduitT (Either l r) r m ()
projectRights = awaitForever $ either (const $ return ()) yield
{-# INLINE projectRights #-}

-- | Propagate every N messages and drop all others.
everyN :: Monad m => Int -> ConduitT a a m ()
everyN n = go 1
  where
    go n' = await >>= maybe (return ()) (\x ->
      if n' < n
        then go (n'+1)
        else yield x >> go 1)
{-# INLINE everyN #-}

-- | Performs an action every N messages, but ignores its result. All original values are propagted downstream.
onEveryN :: Monad m => Int -> (a -> m b) -> ConduitT a a m ()
onEveryN n f = go 1
  where
    go i = await >>= maybe (pure ()) (\x ->
            if i < n
              then yield x >> go (i + 1)
              else lift (f x) >> yield x >> go 1)
{-# INLINE onEveryN #-}

-- | Performs an action every N messages, but ignores its result. All original values are propagted downstream.
onEveryN' :: Monad m => Int -> m b -> ConduitT a a m ()
onEveryN' n m = go 1
  where
    go i = await >>= maybe (pure ()) (\x ->
            if i < n
              then yield x >> go (i + 1)
              else lift m >> yield x >> go 1)
{-# INLINE onEveryN' #-}

-- | Propagate a message every N seconds and drop all others.
everyNSeconds :: MonadIO m => Int -> ConduitT a a m ()
everyNSeconds interval = go 0
  where
    go t = do
      mmsg <- await
      case mmsg of
        Nothing -> pure ()
        Just msg -> do
          ct <- liftIO $ round . T.utcTimeToPOSIXSeconds <$> T.getCurrentTime
          if ct > t
            then yield msg >> go (ct + interval)
            else go t

---------

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
{-# DEPRECATED effect "Use effectC instead" #-}
effect :: Monad m => (a -> m b) -> ConduitT a a m ()
effect = effectC

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effect' :: Monad m => m b -> ConduitT a a m ()
effect' = effectC'

{-# DEPRECATED inJust "Use justC instead" #-}
inJust :: Monad m => ConduitT a c m () -> ConduitT (Maybe a) (Maybe c) m ()
inJust = justC

-- | Sinks values into a given MVar.
mvarSink :: MonadIO m => MVar a -> ConduitT a () m ()
mvarSink mvar = awaitForever $ \v ->
  liftIO $ tryTakeMVar mvar >> putMVar mvar v

-- | Taps the stream by applying a transformation and sending the transformed
-- value into a given sink. The original value is then propagated downstream.
--
-- > tapWith projectLefts myErrorSink

{-# DEPRECATED tapWith "Unsafe.  Do not use" #-}
tapWith :: Monad m => ConduitT a b m () -> ConduitT b Void m () -> ConduitT a a m ()
tapWith f s = passthroughSink (f .| s) (const $ return ())
{-# INLINE tapWith #-}

-- | Taps into a given sink. The original value is then propagated downstream.
{-# DEPRECATED tap "Unsafe.  Do not use" #-}
tap :: Monad m => ConduitT a Void m () -> ConduitT a a m ()
tap s = passthroughSink s (const $ return ())
{-# INLINE tap #-}

-- | Taps a conduit, and sends the results into two different sinks, switching
-- on a predicate.
{-# DEPRECATED tapPred "Unsafe.  Do not use" #-}
tapPred :: Monad m => (a -> Bool) -> ConduitT a Void m () -> ConduitT a Void m () -> ConduitT a a m ()
tapPred p tr fl = tap (L.filter p .| tr) .| tap (L.filter (not . p) .| fl)
{-# INLINE tapPred #-}

-- | For every `Nothing` value in a stream sends `()` to a given `Sink`.
-- Downstream receives an untouched original `Maybe` value.
{-# DEPRECATED tapNothing "Unsafe.  Do not use" #-}
tapNothing :: Monad m => ConduitT () Void m () -> ConduitT (Maybe a) (Maybe a) m ()
tapNothing = tapWith projectNothings
{-# INLINE tapNothing #-}

-- | For every `Nothing` value in a stream sends `()` to a given `Sink`.
-- `Nothing` is then not propagated downstream.
-- Downstream only receives values from `Just`
{-# DEPRECATED divertNothing "Unsafe.  Do not use" #-}
divertNothing :: Monad m => ConduitT () Void m () -> ConduitT (Maybe a) a m ()
divertNothing sink = tapNothing sink .| L.catMaybes
{-# INLINE divertNothing #-}

-- | Sends every left-side value in a stream into a given `Sink`.
-- Downstream receives the original `Either` value untouched.
{-# DEPRECATED tapLeft "Unsafe.  Do not use" #-}
tapLeft :: Monad m => ConduitT l Void m () -> ConduitT (Either l r) (Either l r) m ()
tapLeft = tapWith projectLefts
{-# INLINE tapLeft #-}

-- | Sends every left-side value in a stream into a given `Sink`.
-- Downstream receives only right-side values.
{-# DEPRECATED divertLeft "Unsafe.  Do not use" #-}
divertLeft :: Monad m => ConduitT l Void m () -> ConduitT (Either l r) r m ()
divertLeft sink = tapLeft sink .| projectRights
{-# INLINE divertLeft #-}

-- | Sends every right-side value in a stream into a given `Sink`.
-- Downstream receives the original `Either` value untouched.
{-# DEPRECATED tapRight "Unsafe.  Do not use" #-}
tapRight :: Monad m => ConduitT r Void m () -> ConduitT (Either l r) (Either l r) m ()
tapRight = tapWith projectRights
{-# INLINE tapRight #-}

-- | Sends every right-side value in a stream into a given `Sink`.
-- Downstream receives only left-side values.
{-# DEPRECATED divertRight "Unsafe.  Do not use" #-}
divertRight :: Monad m => ConduitT r Void m () -> ConduitT (Either l r) l m ()
divertRight sink = tapRight sink .| projectLefts
{-# INLINE divertRight #-}
