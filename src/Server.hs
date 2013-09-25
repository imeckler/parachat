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

directory :: TVar (Map Username Addr)
directory = unsafePerformIO (newTVarIO M.empty)

addToDirectory :: Username -> Addr -> STM ()
addToDirectory u = modifyTVar directory . M.insert u

sockAddrToAddr :: SockAddr -> IO (Maybe Addr)
sockAddrToAddr sa = getNameInfo [] True True sa >>| \(x, y) -> (,) <$> x <*> y

main :: IO ()
main =
  NS.serve NS.HostAny serverPort $ \(sock, addr) ->
    forever $ do
      msgMay <- NS.recv sock 4096 >>| bind (decode .> eitherToMaybe)
      may msgMay (return ()) $ \case
        Login user -> do
          print (Login user)
          sockAddrToAddr addr >>= 
            maybe (return ()) (atomically . addToDirectory user)
        GetAddr friend -> do
          putStrLn $ "getting the addr of " ++ friend
          atomically (readTVar directory) >>=
            M.lookup friend .> Friend friend .> encode .> NS.send sock

