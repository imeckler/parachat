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

data UserInfo
  = UserInfo
  { username :: String
  , buddies  :: [String]
  } deriving (Show, Read)

data UserInput
  = Send Username Message
  deriving (Show)

data ConvoState
  = Active
  | Connecting (TMVar Socket)

serverHostName :: IO HostName
serverHostName = head <$> getArgs

configPath :: IO FilePath
configPath = (++ "/.pararc") <$> getHomeDirectory

readUserInfo :: String -> Maybe UserInfo
readUserInfo = maybeRead

getUserInput :: IO (Input UserInput)
getUserInput = do
  (writer, reader) <- spawn Unbounded
  putStrLn "Who you wanna talk to"
  u <- getLine
  forkIO (forever $ atomically . PC.send writer . Send u =<< getLine)
  return reader

may :: Maybe a -> b -> (a -> b) -> b
may m def f = maybe def f m

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
  serverName <- serverHostName
  (serverSock, _) <- N.connectSock serverName "8080"
  N.send serverSock (encode $ Login username)
  return $ getFromServer serverSock

  where
    newSock (host, port) = do
      (sock, _addr) <- connectSock host port
      N.send sock . encode $ Hail username
      return sock

    getFromServer :: Socket -> Username -> IO (Maybe Socket)
    getFromServer serverSock friend = do
      N.send serverSock . encode $ GetAddr friend
      N.recv serverSock 4096 >>=
        bind (decode .> eitherToMaybe)
        .> bind (\(Friend _name addrMay) -> newSock <$> addrMay)
        .> maybeIOSwap

{--
poBoxMaker :: SockMaker -> Input PORequest -> IO (Async (), Input (Username, POBox))
poBoxMaker makeSock reqs = do
  (convoStreamW, convoStreamR) <- spawn Unbounded
  a <- async . runEffect $ fromInput reqs >-> handleReqs convoStreamW
  return (a, convoStreamR)
  where
    handleReqs convoStreamW = forever $
      await >>= \case
        LiveOne friend sock -> liftIO $
          sockBoxes sock >>= 
          (friend,) .> send convoStreamW .> atomically

        OpenPOBox friend -> liftIO $
          makeSock friend >>=
            maybe (return True)
              (sockBoxes >=> (friend,) .> send convoStreamW .> atomically)

    sockBoxes sock = do
      (inW, inR) <- spawn Unbounded
      forkIO . runEffect $ fromSocket sock 4096
        >-> P.map (decode >=> extractMessage)
        >-> P.filter isRight >-> P.map fromRight
        >-> toOutput inW

      (outW, outR) <- spawn Unbounded
      forkIO . runEffect $
        fromInput outR >-> P.map (Message .> encode) >-> toSocket sock
      return (outW, inR)
--}

fromPeer :: Socket -> Producer ToPeer IO ()
fromPeer sock =
  fromSocket sock 4096
  >-> P.map decode
  >-> forever (await >>= either (const $ return ()) yield)

toPeer :: Socket -> Consumer ToPeer IO ()
toPeer sock = forever (await >>= encode .> yield) >-> toSocket sock

{--
main :: IO ()
main = do
  userInfo  <- fromMaybe (error "Could not read config file") . readUserInfo <$> (readFile =<< configPath)
  userInput <- getUserInput
  makeSock  <- sockMaker userInfo
  (poRequestW, poRequestR) <- spawn Unbounded
  
  mapM_ (runEffect .> async >=> wait)
    [ server poRequestW
    , fromInput userInput  >-> forever (mkPORequests poRequestW)
    , fromInput poRequestR >-> postmaster makeSock
    ]

  return ()

  where
    mkPORequests poRequestW = forever $ do
      Send user msg <- await
      liftIO . atomically $ do
        send poRequestW (OpenPOBox user)
        -- send poRequestW (SendReq user msg)
--}

server' :: IO (Event (Username, Socket))
server' = do
  (evt, trigger) <- newEvent
  let handle _ (Message _)    = error "Did not receive hail"
      handle sock (Hail user) = trigger (user, sock)
  N.serve HostAny "8080" $ \(sock, _) ->
    N.recv sock 4096 >>= bind (decode .> eitherToMaybe) .> maybe (return ()) (handle sock)
  return evt

{--
server :: Output PORequest -> Effect IO ()
server poRequestW = do
  N.serve HostAny "8080" $ \(sock, _) -> do
    N.recv sock 4096 >>= bind (decode .> eitherToMaybe) .> maybe (return ()) (handle sock)
  where
    handle _ (Message _) = error "Did not receive hail"
    handle sock (Hail user) = do
      print (Hail user)
      atomically $ PC.send poRequestW (LiveOne user sock)
      liftIO . runEffect $ fromSocket sock 4096 >-> P.print
--}
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
