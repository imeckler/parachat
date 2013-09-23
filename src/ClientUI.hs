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
import Data.Monoid
-- import Control.Applicative

import qualified Data.Map as M
import Data.Map (Map)
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

setupGUI :: Window -> IO ()
setupGUI window = do
  pure window # set UI.title "Parachat"
  let bod = getBody window
  return ()

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
  = HookItUp (Maybe Sock) ChatUI
  | Close Username

-- The Event returned is ChatUI's that need to be hooked up to sockets
mkChatArea :: Event (Username, Socket)
           -> Element
           -> IO (Event PORequest) 
mkChatArea incoming container = do
  (newChatEntry, entries) <- mkNewChatEntry
  let requestedUIs   = unsafeMapIO mkChatBox entries
      incomingPOReqs = unsafeMapIO (\(u, s) -> HookItUp (Just s) <$> mkChatBox u) incoming
      -- consider getting rid of this
      incomingUIs    = fmap (\(HookItUp _ ui) -> ui) incomingPOReqs

  register (unions [requestedUIs, incomingUIs]) ((container #+) . map _elt)

  removals <- bindS requestedUIs $ \(ChatUI {..}) -> fmap (const buddyName) closes
  return (fmap (HookItUp Nothing) requestedUIs <> incomingPOReqs <> fmap Close removals)

postmaster :: SockMaker -> Event PORequest -> IO ()
postmaster makeSock reqs = do
  register reqs $ \case
    HookItUp sockMay (ChatUI {..}) -> do
      sockMay' <- maybe (makeSock buddyName) (return . Just) sockMay
      may sockMay' (return ()) $ \sock ->
        sinkMessages . producerToEvent $ P.map  fromSocket sock 4096

main :: IO ()
main = return (){-- do
  (newChatEntry, entries) <- mkNewChatEntry
  (friend, --}

