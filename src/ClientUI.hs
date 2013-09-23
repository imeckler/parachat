{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase #-}
module Main where

import Utils
import Protocol
import Client hiding (PORequest(..))

import Reactive.Threepenny
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny.Core
-- import Control.Applicative

import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import Data.Maybe
import Data.Thyme
import qualified Network.Socket as S
import Network.Socket (Socket)
import Data.IORef

import Frp

data ChatUI =
  ChatUI {  _elt        :: Element
         , buddyName    :: String
         -- Consider using Input rather than Event to ensure no messages are dropped
         , sentMessages :: Event (UTCTime, String)
         , sinkMessages :: Event (UTCTime, String) -> IO (IO ())
         , closes       :: Event ()
         }

getValueAndClear :: Element -> IO String
getValueAndClear elt = get value elt <* set value "" (pure elt)

mkNewChatEntry :: IO (Element, Event Username)
mkNewChatEntry = do
  newChatEntry <- UI.input # set type_ "text"
  let entries =  filterE (== 13) (keydown newChatEntry)
              |> fmap (const ())
              |> mappend (blur newChatEntry) 
              |> unsafeMapIO (\_ -> getValueAndClear newChatEntry)
              |> filterE (not . null)

  return (newChatEntry, entries)

data ChatAction = NewChat Username | FromFriend POBox

mkChatBox :: Username -> IO ChatUI -- (Element, Event String, Trigger String)
mkChatBox buddyName = do
  container   <- UI.div # set class_ "chatContainer"
  messages    <- UI.div # set class_ "chatMessages"
  msgInput    <- UI.input # set type_ "text" # set class_ "msgInput"
  closeButton <- UI.button #+ [string "Close chat"]

  pure container #+ [pure messages, pure msgInput, pure closeButton]

  let closes = click closeButton
  register closes $ \_ -> delete container

  return $ ChatUI
    { buddyName
    , _elt         = container
    , sentMessages =
        unsafeMapIO (\_ -> (,) <$> getCurrentTime <*> getValueAndClear msgInput)
        $ filterE (== 13) (keydown msgInput) 
    , sinkMessages = \ms -> register ms (\m -> () <$ pure messages #+ [mkMessageElt m])
    , closes
    }

  where mkMessageElt (t, msg) = string (show t ++ ": " ++ msg)

-- The argument is incoming chats, from the listener-server

data PORequest
  = HookItUp (Maybe Socket) ChatUI
  | Close Username

-- The Event returned is ChatUI's that need to be hooked up to sockets
mkChatArea :: Event (Username, Socket)
           -> Element
           -> IO (Event PORequest) 
mkChatArea incoming container = do
  (newChatEntry, entries) <- mkNewChatEntry
  pure container #+ [pure newChatEntry]
  let requestedUIs   = unsafeMapIO mkChatBox entries
      incomingPOReqs = unsafeMapIO (\(u, s) -> HookItUp (Just s) <$> mkChatBox u) incoming
      -- consider getting rid of this
      incomingUIs    = fmap (\(HookItUp _ ui) -> ui) incomingPOReqs

  register (unions [requestedUIs, incomingUIs])
           (ignoreM . (pure container #+) . map (pure . _elt))

  removals <- bindS requestedUIs $ \(ChatUI {..}) -> fmap (const buddyName) closes
  return (fmap (HookItUp Nothing) requestedUIs <> incomingPOReqs <> fmap Close removals)

postmaster :: SockMaker -> Event PORequest -> IO ()
postmaster makeSock reqs = do
  socks <- newIORef M.empty
  let makeSock' u  = do 
        sockMay <- makeSock u
        maybe (pure ()) (modifyIORef socks . M.insert u) sockMay
        return sockMay

      obtainSock u = do
        sockMay <- M.lookup u <$> readIORef socks
        case sockMay of
          Nothing -> makeSock' u
          Just s  -> S.isConnected s >>= boolElim (return (Just s)) (makeSock' u)

  register reqs $ \case
    HookItUp sockMay (ChatUI {..}) -> do
      sockMay' <- maybe (obtainSock buddyName) (return . Just) sockMay
      may sockMay' (return ()) $ \sock -> ignoreM $ do
        fromPeer sock >>= sinkMessages
        toPeer sock sentMessages

    Close u -> do
      (sockMay, socks') <- M.updateLookupWithKey (\_ _ -> Nothing) u <$> readIORef socks
      maybe (pure ()) S.close sockMay
      writeIORef socks socks'

  return ()


main :: IO ()
main = do
  userInfo <- fromMaybe (error "Could not read config file") . readUserInfo <$> (readFile =<< configPath)
  startGUI defaultConfig (setupGUI userInfo)

setupGUI :: UserInfo -> Window -> IO ()
setupGUI userInfo window = do
  pure window # set UI.title "Parachat"
  incoming <- server
  makeSock <- sockMaker userInfo

  chatContainer <- UI.div # set class_ "chatContainer"
  getBody window #+ [pure chatContainer]

  poRequests <- mkChatArea incoming chatContainer
  postmaster makeSock poRequests

removable :: (a -> IO Element)
          -> (Element -> Event ())
          -> Event a
          -> Element
          -> IO (Behavior [(a, Element)])
removable mkElem mkRemovals adds container = do
  idx                <- newIORef (0 :: Int)
  (changes, trigger) <- newEvent
  register adds $ \x -> do
    i   <- readIORef idx
    elt <- mkElem x
    modifyIORef idx (+ 1)
    trigger (M.insert i (x, elt))
    pure container #+ [pure elt]
    register (mkRemovals elt) $ \() -> do
      delete elt
      trigger (M.delete i)
    return ()

  fmap M.elems <$> accumB M.empty changes


