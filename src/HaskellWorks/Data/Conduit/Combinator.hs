module HaskellWorks.Data.Conduit.Combinator where

import           Control.Concurrent     (MVar, putMVar, tryTakeMVar)
import           Control.Monad          (void)
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List      as L
import           Data.Maybe
import           Data.Time.Clock.POSIX  as T

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effect :: Monad m => (a -> m b) -> Conduit a m a
effect f = L.mapM (\a -> f a >> return a)

-- | Performs the effect but ignores its result.
-- The original value is propagated downstream.
effect' :: Monad m => m b -> Conduit a m a
effect' m = L.mapM (\a -> m >> return a)

inJust :: Monad m => Conduit a m c -> Conduit (Maybe a) m (Maybe c)
inJust c = getZipConduit
      $   ZipConduit (L.filter isNothing  .|      L.map (const Nothing))
      <*  ZipConduit (L.concat            .| c .| L.map Just           )

-- | Sinks values into a given MVar.
mvarSink :: MonadIO m => MVar a -> Sink a m ()
mvarSink mvar = awaitForever $ \v ->
  liftIO $ tryTakeMVar mvar >> putMVar mvar v

-- | Taps the stream by applying a transformation and sending the transformed
-- value into a given sink. The original value is then propagated downstream.
--
-- > tapWith projectLefts myErrorSink
tapWith :: Monad m => Conduit a m b -> Sink b m () -> Conduit a m a
tapWith f s = passthroughSink (f .| s) (const $ return ())
{-# INLINE tapWith #-}

-- | Taps into a given sink. The original value is then propagated downstream.
tap :: Monad m => Sink a m () -> Conduit a m a
tap s = passthroughSink s (const $ return ())
{-# INLINE tap #-}

-- | Taps a conduit, and sends the results into two different sinks, switching
-- on a predicate.
tapPred :: Monad m => (a -> Bool) -> Sink a m () -> Sink a m () -> Conduit a m a
tapPred p tr fl =
  tap (L.filter p .| tr) .| tap (L.filter (not . p) .| fl)
{-# INLINE tapPred #-}

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

-- | For every `Nothing` value in a stream sends `()` to a given `Sink`.
-- Downstream receives an untouched original `Maybe` value.
tapNothing :: Monad m => Sink () m () -> Conduit (Maybe a) m (Maybe a)
tapNothing = tapWith projectNothings
{-# INLINE tapNothing #-}

-- | For every `Nothing` value in a stream sends `()` to a given `Sink`.
-- `Nothing` is then not propagated downstream.
-- Downstream only receives values from `Just`
divertNothing :: Monad m => Sink () m () -> Conduit (Maybe a) m a
divertNothing sink = tapNothing sink .| L.catMaybes
{-# INLINE divertNothing #-}

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

-- | Sends every left-side value in a stream into a given `Sink`.
-- Downstream receives the original `Either` value untouched.
tapLeft :: Monad m => Sink l m () -> Conduit (Either l r) m (Either l r)
tapLeft = tapWith projectLefts
{-# INLINE tapLeft #-}

-- | Sends every left-side value in a stream into a given `Sink`.
-- Downstream receives only right-side values.
divertLeft :: Monad m => Sink l m () -> Conduit (Either l r) m r
divertLeft sink = tapLeft sink .| projectRights
{-# INLINE divertLeft #-}

-- | Sends every right-side value in a stream into a given `Sink`.
-- Downstream receives the original `Either` value untouched.
tapRight :: Monad m => Sink r m () -> Conduit (Either l r) m (Either l r)
tapRight = tapWith projectRights
{-# INLINE tapRight #-}

-- | Sends every right-side value in a stream into a given `Sink`.
-- Downstream receives only left-side values.
divertRight :: Monad m => Sink r m () -> Conduit (Either l r) m l
divertRight sink = tapRight sink .| projectLefts
{-# INLINE divertRight #-}

everyN :: Monad m => Int -> Conduit a m a
everyN n = go 1
  where
    go n' = await >>= maybe (return ()) (\x ->
      if n' < n
        then go (n'+1)
        else yield x >> go 1)
{-# INLINE everyN #-}

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
