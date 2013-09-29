{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, LambdaCase #-}
module Client where

import Control.Applicative
import Control.Monad
-- import Control.Monad.State
-- import Data.Maybe
import System.Directory
import System.Environment
-- import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent as PC
import qualified Pipes.Prelude as P
import Pipes.Network.TCP.Safe hiding (send, recv)
-- import qualified Data.ByteString as B
import Data.Serialize hiding (get)
-- import qualified Network.Socket as S
import qualified Network.Simple.TCP as N
-- import qualified Data.Map as M
-- import Data.Map (Map)
import Control.Concurrent.STM
import Reactive.Threepenny

import Protocol
import Utils
import Frp

data UserInfo
  = UserInfo
  { username :: Username
  , buddies  :: [Username]
  } deriving (Show, Read)

data UserInput
  = Send Username Message
  deriving (Show)

data ConvoState
  = Active
  | Connecting (TMVar Socket)

configPath :: IO FilePath
configPath = head <$> getArgs
-- configPath = (++ "/.pararc") <$> getHomeDirectory

readUserInfo :: String -> Maybe UserInfo
readUserInfo = maybeRead

type POBox = (Output String, Input String)

data PORequest
  = OpenPOBox Username 
  | LiveOne Username Socket
--  | SendReq Username Message

type SockMaker = Username -> IO (Maybe Socket)

maybeIOSwap :: Maybe (IO a) -> IO (Maybe a)
maybeIOSwap = maybe (return Nothing) (fmap Just)

sockMaker :: UserInfo -> IO SockMaker
sockMaker (UserInfo {..}) = do
  (serverSock, _) <- N.connectSock serverName serverPort
  N.send serverSock (encode $ Login username)
  return $ getFromServer serverSock

  where
    newSock host = do
      putStrLn "newSock"
      (sock, _addr) <- connectSock host clientPort
      N.send sock . encode $ Hail username
      return sock

    getFromServer :: Socket -> Username -> IO (Maybe Socket)
    getFromServer serverSock friend = do
      N.send serverSock . encode $ GetAddr friend
      N.recv serverSock 4096 >>=
        bind (decode .> eitherToMaybe)
        .> bind (\(Friend _name addrMay) -> newSock . fst <$> addrMay)
        .> maybeIOSwap

toPeer :: Serialize a => Socket -> Event a -> IO (IO ())
toPeer sock = flip register (encode .> N.send sock)

fromPeer :: Serialize a => Socket -> IO (Event a)
fromPeer sock = producerToEvent $
  fromSocket sock 4096
  >-> P.map decode
  >-> forever (await >>= either (\_ -> pure ()) yield)

clientPort :: ServiceName
clientPort = "8081"

mkIncoming :: IO (Event (Username, Socket))
mkIncoming = do
  (evt, trigger) <- newEvent
  let handle _    (Message _) = error "Did not receive hail"
      handle sock (Hail user) = trigger (user, sock)
  forkIO . N.serve HostAny clientPort $ \(sock, _) ->
    N.recv sock 4096 >>= bind (decode .> eitherToMaybe) .> maybe (return ()) (handle sock)
  return evt


