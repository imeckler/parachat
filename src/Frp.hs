module Frp where

import Reactive.Threepenny
import Pipes
import Pipes.Concurrent
import Control.Monad
import Data.Monoid

instance Monoid (Event a) where
  mappend = unionWith const
  mempty  = never

eventToInput :: Event a -> IO (Input a)
eventToInput e = do
  (writer, reader) <- spawn Unbounded
  register e (fmap (const ()) . atomically . send writer)
  return reader

inputToEvent :: Input a -> IO (Event a)
inputToEvent inp = do
  (evt, trigger) <- newEvent
  forkIO . runEffect $ fromInput inp >-> forever (await >>= liftIO . trigger)
  return evt


