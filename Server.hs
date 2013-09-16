module Server where

import Protocol
import Control.Proxy
import System.IO (Handle)
import qualified System.IO as IO
import Data.Serialize
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

hPutByteStringD :: (Proxy p) => Handle -> x -> p x B.ByteString x B.ByteString IO r
hPutByteStringD h = runIdentityK $ foreverK $ \x -> do
  a <- request x
  lift $ B.hPutStr h a
  respond a
{-# INLINABLE hPutByteStringD #-}

hSerializeD :: (Proxy p, Serialize a) => Handle -> c' -> p c' a c' B.ByteString IO r
hSerializeD h = mapD (runPut . put) >-> hPutByteStringD h

hUnserialize :: (Proxy p, Serialize a) => Handle -> () -> Producer p a IO ()
hUnserialize h () = runIdentityP (lift (LB.hGetContents h) >>= go) where
  go text =
    either (const $ return ())
           (\(x, text') -> respond x >> go text')
           (runGetLazyState get text)

main = do
  h <- IO.openFile "foo" IO.AppendMode
  return ()
