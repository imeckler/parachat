{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}

module Client where

import Control.Applicative
import Control.Monad
import Control.Monad.State
-- import Data.Maybe
import Pipes
import Pipes.Concurrent as PC
-- import qualified Pipes.Prelude as P
import Pipes.Network.TCP.Safe
-- import qualified Data.ByteString as B
import Data.Serialize
-- import qualified Network.Socket as S
import qualified Network.Simple.TCP as N
import qualified Data.Map as M
import Data.Map (Map)
import Control.Concurrent.STM
import System.IO.Unsafe
import Protocol
import Utils

data UserInfo
  = UserInfo
  { username :: String
  , buddies  :: [String]
  }

type Message  = String
type Username = String

prepareMessage :: String -> String
prepareMessage s = dropFromBack (length deletes) prefix ++ prepareMessage rest'
  where
    (prefix, rest)   = break (== '\DEL') s
    (deletes, rest') = span  (== '\DEL') rest
    dropFromBack n   = reverse . drop n . reverse
                  

data UserInput
  = Send Username Message
  deriving (Show)

serverHostName :: HostName
serverHostName = "localhost"

configPath :: FilePath
configPath = "~/.pararc"

readUserInfo :: String -> Maybe UserInfo
readUserInfo = undefined
{--
userInput :: (MonadIO m) => Producer' UserInput m ()
userInput = do
  liftIO $ putStrLn "Who you wanna talk to"
  liftIO getLine >>= go
  where
    go user = forever $
      liftIO getLine >>= yield . Send user . prepareMessage
--}
getUserInput :: IO (Input UserInput)
getUserInput = do
  (writer, reader) <- spawn Unbounded
  putStrLn "Who you wanna talk to"
  u <- getLine
  forkIO (forever $ atomically . PC.send writer . Send u =<< getLine)
  return reader

openConvos :: TVar (Map Username Socket)
openConvos = unsafePerformIO (newTVarIO M.empty)

type POBox = (Output ToPeer, Input ToPeer)
postOffice :: TVar (Map Username POBox)
postOffice = unsafePerformIO (newTVarIO M.empty)

findPOBox :: Username -> STM (Maybe POBox)
findPOBox user = readTVar postOffice >>| M.lookup user

data PORequest
  = NewPOBox Username 

postmaster :: Consumer PORequest IO ()
postmaster = do
  findSock <- liftIO directory
  forever do
    NewPOBox user <- await
    atomically (findPOBox user) >>=
      maybe (newPOBox findSock user) (const $ return ())
  where
    newPOBox newSock user = do
      poBox <- spawn Unbounded
      sock  <- newSock user
      atomically $ modifyTVar (M.insert user poBox)



mailman :: (Username -> IO (Maybe Socket)) -> UserInfo -> Consumer UserInput IO ()
mailman findSock userInfo = do
  forever $ do
    Send friend msg <- await
    liftIO $
      findSock friend >>=
      maybe (return ()) (flip N.send (encode $ Message msg))

mailman :: UserInfo -> Input UserInput -> IO Effect
mailman userInfo inbox =
  directory >>| \findSock ->
    fromInput input >-> sendMail findSock
  where
    sendMail findSock = do
      Send friend msg <- await
      

sockMaker :: IO (Username -> IO Socket)
sockMaker :: IO (Output Socket)
sockMaker = do
  (sockRequestsW, sockRequestsR) <- spawn Unbounded

  (serverSock, _) <- N.connectSock serverHostName "8080"
  N.send serverSock (encode $ Login username)
  return $ \friend -> do
    N.send serverSock . encode $ GetAddr friend
    


maybeIOSwap :: Maybe (IO a) -> IO (Maybe a)
maybeIOSwap = maybe (return Nothing) (fmap Just)

directory :: UserInfo -> IO (Username -> IO (Maybe Socket))
directory (UserInfo {..}) = do
  (serverSock, _) <- N.connectSock serverHostName "8080"
  N.send serverSock (encode $ Login username)

  return $ \friend -> do
    atomically (readTVar openConvos) >>=
      M.lookup friend .> maybe (getFromServer serverSock friend) (return . Just)
  where
    newSock name (host, port) = do
      (sock, _) <- connectSock host port
      atomically $ modifyTVar openConvos (M.insert name sock)
      return sock

    getFromServer :: Socket -> Username -> IO (Maybe Socket)
    getFromServer serverSock friend = do
      N.send serverSock . encode $ GetAddr friend
      N.recv serverSock 4096 >>=
        bind (decode .> eitherToMaybe)
        .> bind (\(Friend name addrMay) -> newSock name <$> addrMay)
        .> maybeIOSwap

iterPipe :: (MonadIO m) => (a -> IO ()) -> Consumer a m ()
iterPipe f = await >>= liftIO . f >> iterPipe f

clientPort :: ServiceName
clientPort = "8080"

{--
main :: IO ()
main = do
  userInfo    <- fromMaybe (error "Could not read config file") . readUserInfo <$> readFile configPath
  userInput   <- getUserInput
  runEffect $ fromInput userInput >-> handleUserInput
  return ()


handleUserInput :: Input UserInput -> IO ()
handleUserInput input = do
  (dirReader, dirWriter) <- spawn Unbounded

userInputToServer :: UserInput -> ToServer
userInputToServer (Send user msg) = 

main :: IO ()
main = do
  (msgOut, msgIn) <- span Unbounded
  a <- async $ getUserInput msgOut

  (serverSock, serverAddr) <- N.connectSock serverHostName "8080"
  runEffect $ userInput >-> P.map (requestAddr .> put .> runPut) >-> toSocket serverSock
  -- let fromServer = fromSocket serverSock 4096
  return ()
--}
