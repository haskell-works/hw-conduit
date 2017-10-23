module HaskellWorks.Data.Conduit.Combinator where

import Control.Concurrent     (MVar, putMVar, tryTakeMVar)
import Control.Monad          (void)
import Control.Monad.IO.Class
import Data.Conduit
import Data.Maybe
import Data.Time.Clock.POSIX  as T

import qualified Data.Conduit.List as L

-- | Run the provided conduit in the Just case
maybeC :: Monad m => Conduit () m () -> Conduit a m c -> Conduit (Maybe a) m (Maybe c)
maybeC n j = getZipConduit
  $   ZipConduit (L.filter isNothing  .| L.map (const ()) .|  n .| L.map (const Nothing))
  <*  ZipConduit (L.concat            .|                      j .| L.map Just           )

-- | Run the provided conduit in the Just case
justC :: Monad m => Conduit a m c -> Conduit (Maybe a) m (Maybe c)
justC = maybeC (L.map id)

-- | Run the provided conduit in the Just case
nothingC :: Monad m => Conduit () m () -> Conduit (Maybe a) m (Maybe a)
nothingC n = maybeC n (L.map id)

-- | Run the provided conduits on the left and right side of the either respectively
eitherC :: Monad m => Conduit l m a -> Conduit r m a -> Conduit (Either l r) m a
eitherC l r = getZipConduit
  $   ZipConduit (projectLefts  .| l)
  <*  ZipConduit (projectRights .| r)

-- | Run the conduit on the right side of the either
rightC :: Monad m => Conduit r m a -> Conduit (Either l r) m (Either l a)
rightC r = eitherC (L.map Left) (r .| L.map Right)

-- | Run the conduit on the left side of the either
leftC :: Monad m => Conduit l m a -> Conduit (Either l r) m (Either a r)
leftC l = eitherC (l .| L.map Left) (L.map Right)

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effectC :: Monad m => (a -> m b) -> Conduit a m a
effectC f = L.mapM (\a -> f a >> return a)

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effectC' :: Monad m => m b -> Conduit a m a
effectC' m = L.mapM (\a -> m >> return a)

-- | Creates a unified sink, which is actually two separate sinks with results
-- being sent to one or the other based on a predicate.
sinkWithPred :: Monad m => (a -> Bool) -> Sink a m () -> Sink a m () -> Sink a m ()
sinkWithPred p tr fl =
  void $ sequenceSinks [L.filter p .| tr, L.filter (not . p) .| fl]
{-# INLINE sinkWithPred #-}

-- | Projects nothings from the stream.
-- Returns a stream that only contains nothings (represented by unit)
projectNothings :: Monad m => Conduit (Maybe a) m ()
projectNothings = awaitForever $ maybe (yield ()) (const $ return ())
{-# INLINE projectNothings #-}

-- | Projects left side values for each value in a stream.
-- Downstream only receives values that were on the left side,
-- the right side is ignored.
projectLefts :: Monad m => Conduit (Either l r) m l
projectLefts = awaitForever $ either yield (const $ return ())
{-# INLINE projectLefts #-}

-- | Projects right side values for each value in a stream.
-- Downstream only receives values that were on the right side,
-- the left side is ignored.
projectRights :: Monad m => Conduit (Either l r) m r
projectRights = awaitForever $ either (const $ return ()) yield
{-# INLINE projectRights #-}

-- | Propagate every N messages and drop all others.
everyN :: Monad m => Int -> Conduit a m a
everyN n = go 1
  where
    go n' = await >>= maybe (return ()) (\x ->
      if n' < n
        then go (n'+1)
        else yield x >> go 1)
{-# INLINE everyN #-}

-- | Propagate a message every N seconds and drop all others.
everyNSeconds :: MonadIO m => Int -> Conduit a m a
everyNSeconds interval = go 0
  where
    go t = do
      mmsg <- await
      case mmsg of
        Nothing -> pure ()
        Just msg -> do
          ct <- liftIO $ (round . T.utcTimeToPOSIXSeconds) <$> T.getCurrentTime
          if ct > t
            then yield msg >> go (ct + interval)
            else go t

---------

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
{-# DEPRECATED effect "Use effectC instead" #-}
effect :: Monad m => (a -> m b) -> Conduit a m a
effect = effectC

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effect' :: Monad m => m b -> Conduit a m a
effect' = effectC'

{-# DEPRECATED inJust "Use justC instead" #-}
inJust :: Monad m => Conduit a m c -> Conduit (Maybe a) m (Maybe c)
inJust = justC

-- | Sinks values into a given MVar.
mvarSink :: MonadIO m => MVar a -> Sink a m ()
mvarSink mvar = awaitForever $ \v ->
  liftIO $ tryTakeMVar mvar >> putMVar mvar v

-- | Taps the stream by applying a transformation and sending the transformed
-- value into a given sink. The original value is then propagated downstream.
--
-- > tapWith projectLefts myErrorSink

{-# DEPRECATED tapWith "Unsafe.  Do not use" #-}
tapWith :: Monad m => Conduit a m b -> Sink b m () -> Conduit a m a
tapWith f s = passthroughSink (f .| s) (const $ return ())
{-# INLINE tapWith #-}

-- | Taps into a given sink. The original value is then propagated downstream.
{-# DEPRECATED tap "Unsafe.  Do not use" #-}
tap :: Monad m => Sink a m () -> Conduit a m a
tap s = passthroughSink s (const $ return ())
{-# INLINE tap #-}

-- | Taps a conduit, and sends the results into two different sinks, switching
-- on a predicate.
{-# DEPRECATED tapPred "Unsafe.  Do not use" #-}
tapPred :: Monad m => (a -> Bool) -> Sink a m () -> Sink a m () -> Conduit a m a
tapPred p tr fl =
  tap (L.filter p .| tr) .| tap (L.filter (not . p) .| fl)
{-# INLINE tapPred #-}

-- | For every `Nothing` value in a stream sends `()` to a given `Sink`.
-- Downstream receives an untouched original `Maybe` value.
{-# DEPRECATED tapNothing "Unsafe.  Do not use" #-}
tapNothing :: Monad m => Sink () m () -> Conduit (Maybe a) m (Maybe a)
tapNothing = tapWith projectNothings
{-# INLINE tapNothing #-}

-- | For every `Nothing` value in a stream sends `()` to a given `Sink`.
-- `Nothing` is then not propagated downstream.
-- Downstream only receives values from `Just`
{-# DEPRECATED divertNothing "Unsafe.  Do not use" #-}
divertNothing :: Monad m => Sink () m () -> Conduit (Maybe a) m a
divertNothing sink = tapNothing sink .| L.catMaybes
{-# INLINE divertNothing #-}

-- | Sends every left-side value in a stream into a given `Sink`.
-- Downstream receives the original `Either` value untouched.
{-# DEPRECATED tapLeft "Unsafe.  Do not use" #-}
tapLeft :: Monad m => Sink l m () -> Conduit (Either l r) m (Either l r)
tapLeft = tapWith projectLefts
{-# INLINE tapLeft #-}

-- | Sends every left-side value in a stream into a given `Sink`.
-- Downstream receives only right-side values.
{-# DEPRECATED divertLeft "Unsafe.  Do not use" #-}
divertLeft :: Monad m => Sink l m () -> Conduit (Either l r) m r
divertLeft sink = tapLeft sink .| projectRights
{-# INLINE divertLeft #-}

-- | Sends every right-side value in a stream into a given `Sink`.
-- Downstream receives the original `Either` value untouched.
{-# DEPRECATED tapRight "Unsafe.  Do not use" #-}
tapRight :: Monad m => Sink r m () -> Conduit (Either l r) m (Either l r)
tapRight = tapWith projectRights
{-# INLINE tapRight #-}

-- | Sends every right-side value in a stream into a given `Sink`.
-- Downstream receives only left-side values.
{-# DEPRECATED divertRight "Unsafe.  Do not use" #-}
divertRight :: Monad m => Sink r m () -> Conduit (Either l r) m l
divertRight sink = tapRight sink .| projectLefts
{-# INLINE divertRight #-}
