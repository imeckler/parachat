module Frp where

import Reactive.Threepenny
import Pipes
import Pipes.Concurrent
import Control.Monad
import Data.Monoid

instance Monoid (Event a) where
  mappend = unionWith const
  mempty  = never

asyncly :: (a -> IO b) -> IO (Output a, Input b)
asyncly f = do
  (writerIn, readerIn)   <- spawn Unbounded
  (writerOut, readerOut) <- spawn Unbounded
  forkIO . runEffect $
    fromInput readerIn
    >-> forever (await >>= liftIO . f >>= yield)
    >-> toOutput writerOut
  return (writerIn, readerOut)

eventToInput :: Event a -> IO (Input a)
eventToInput e = do
  (writer, reader) <- spawn Unbounded
  register e (fmap (const ()) . atomically . send writer)
  return reader

inputToEvent :: Input a -> IO (Event a)
inputToEvent = producerToEvent . fromInput

producerToEvent :: Producer a IO () -> IO (Event a)
producerToEvent p = do
  (evt, trigger) <- newEvent
  forkIO . runEffect $ p >-> forever (await >>= liftIO . trigger)
  return evt

eventToProducer :: MonadIO m => Event a -> IO (Producer a m ())
eventToProducer = fmap fromInput . eventToInput

mapIO :: (a -> IO b) -> Event a -> IO (Event b)
mapIO f e = do
  (evt, trigger)   <- newEvent
  (writer, reader) <- spawn Unbounded
  register e ((() <$) . atomically . send writer)
  forkIO . runEffect $
    fromInput reader >-> forever (await >>= liftIO . (f >=> trigger))
  return evt

scan :: b -> (b -> a -> b) -> Event a -> IO (Behavior b)
scan z f es = accumB z (fmap (flip f) es)

bindS :: Event a -> (a -> Event b) -> IO (Event b)
bindS es f = do
  (evt, trigger) <- newEvent
  register es $ \x -> register (f x) trigger >> return ()
  return evt

type Trigger a = Handler a

