{-# LANGUAGE NoMonomorphismRestriction, LambdaCase #-}
module Main where

import Protocol
import Utils
-- import Pipes
-- import System.IO (Handle)
-- import qualified System.IO as IO
import Data.Serialize
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Map (Map)
import System.IO.Unsafe
import Network.Socket hiding (bind)
import qualified Network.Simple.TCP as NS
-- import Control.Concurrent.Async
import Control.Monad
import Control.Applicative

{--
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
--}

directory :: TVar (Map Username Addr)
directory = unsafePerformIO (newTVarIO M.empty)

addToDirectory :: Username -> Addr -> STM ()
addToDirectory u = modifyTVar directory . M.insert u

sockAddrToAddr :: SockAddr -> IO (Maybe Addr)
sockAddrToAddr sa = getNameInfo [] True True sa >>| \(x, y) -> (,) <$> x <*> y

main :: IO ()
main =
  NS.serve NS.HostAny "8080" $ \(sock, addr) -> do
    forever $ do
      msgMay <- NS.recv sock 4096 >>| bind (decode .> eitherToMaybe)
      may msgMay (return ()) $ \case
        Login user -> do
          print (Login user)
          sockAddrToAddr addr >>= 
            maybe (return ()) (atomically . addToDirectory user)
        GetAddr friend ->
          atomically (readTVar directory) >>=
            M.lookup friend .> Friend friend .> encode .> NS.send sock

