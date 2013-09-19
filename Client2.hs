{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, LambdaCase #-}
module Client2 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent as PC
import qualified Pipes.Prelude as P
import Pipes.Network.TCP.Safe hiding (send, recv)
-- import qualified Data.ByteString as B
import Data.Serialize hiding (get)
-- import qualified Network.Socket as S
import qualified Network.Simple.TCP as N
import qualified Data.Map as M
-- import Data.Map (Map)
import Control.Concurrent.STM
import Protocol
import Utils

data UserInfo
  = UserInfo
  { username :: String
  , buddies  :: [String]
  }

data UserInput
  = Send Username Message
  deriving (Show)

data ConvoState
  = Active
  | Connecting (TMVar Socket)

serverHostName :: HostName
serverHostName = "localhost"

configPath :: FilePath
configPath = "~/.pararc"

readUserInfo :: String -> Maybe UserInfo
readUserInfo = undefined

getUserInput :: IO (Input UserInput)
getUserInput = do
  (writer, reader) <- spawn Unbounded
  putStrLn "Who you wanna talk to"
  u <- getLine
  forkIO (forever $ atomically . PC.send writer . Send u =<< getLine)
  return reader

may :: Maybe a -> b -> (a -> b) -> b
may m def f = maybe def f m

data PORequest
  = OpenPOBox Username 
  | LiveOne Username Socket
  | SendReq Username Message

type SockMaker = Username -> IO (Maybe Socket)

maybeIOSwap :: Maybe (IO a) -> IO (Maybe a)
maybeIOSwap = maybe (return Nothing) (fmap Just)

sockMaker :: UserInfo -> IO SockMaker
sockMaker (UserInfo {..}) = do
  (serverSock, _) <- N.connectSock serverHostName "8080"
  N.send serverSock (encode $ Login username)
  return $ getFromServer serverSock

  where
    newSock (host, port) = fst <$> connectSock host port

    getFromServer :: Socket -> Username -> IO (Maybe Socket)
    getFromServer serverSock friend = do
      N.send serverSock . encode $ GetAddr friend
      N.recv serverSock 4096 >>=
        bind (decode .> eitherToMaybe)
        .> bind (\(Friend _name addrMay) -> newSock <$> addrMay)
        .> maybeIOSwap

postmaster :: MonadIO m => (Username -> IO (Maybe Socket)) -> Consumer PORequest m ()
postmaster makeSock = flip evalStateT M.empty . forever $
  lift await >>= \case
    LiveOne friend sock -> do
      liftIO . forkIO . runEffect $ fromSocket sock 4096 >-> P.print
      modify (M.insert friend sock)

    OpenPOBox user -> do
      isConnected <-
        liftIO . maybe (return False) sIsConnected . M.lookup user <$> get
      when (not isConnected) $
        liftIO (makeSock user) >>= maybe (return ()) (modify . M.insert user)

    SendReq user msg ->
      get >>= M.lookup user
           .> maybe (return ()) (flip N.send (encode $ Message msg))

main :: IO ()
main = do
  userInfo  <- fromMaybe (error "Could not read config file") . readUserInfo <$> readFile configPath
  userInput <- getUserInput
  makeSock  <- sockMaker userInfo
  (poRequestW, poRequestR) <- spawn Unbounded
  
  mapM_ (wait . async . runEffect) $
    [ fromInput userInput >-> forever (mkPORequests poRequestW)
    , server poRequestW
    , fromInput poRequestR >-> postmaster makeSock
    ]

  return ()

  where
    mkPORequests poRequestW = forever $ do
      Send user msg <- await
      liftIO . atomically $ do
        send poRequestW (OpenPOBox user)
        send poRequestW (SendReq user msg)

server :: Output PORequest -> Effect
server poRequestW = do
  serve HostAny "8080" $ \(sock, addr) -> do
  N.recv sock 4096 >>= bind decode .> maybe (return ()) $ \case
    Message _ -> error "Did not receive hail"
    Hail user -> 
  forkIO . runEffect $ fromSocket sock >-> P.print

{--
updateDirectory :: Consumer ToClient IO ()
updateDirectory = do
  Friend name addrMay <- await
  may addrMay (liftIO . putStrLn $ "User " ++ name ++ " not found") $ \addr -> do
    dir <- liftIO . atomically $ readTVar directory

    case M.lookup name dir of
         Just Active         -> return ()
         Just (Connecting _) -> return ()
         Nothing -> do

    (outbox, inbox) <- spawn
    forkIO $ fromInput inbox >-> P.print

    openConnectionToUser name

dispatch :: UserInput -> IO ()
dispatch (Send friend msg) = do
  sockBox <- newEmptyTMVarIO
  getSock sockBox

directory :: TVar (Map Username ConvoState)
--}
