{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving #-}

module Protocol (
  ToServer(..),
  ToClient(..),
  ToPeer(..),
  Username,
  Message,
  Addr,
  -- TODO: Delete
  serverName,
  serverPort
  ) where

import GHC.Generics
import Data.Typeable
import Data.Serialize
import Data.Thyme
import Data.Int
import Unsafe.Coerce

-- import Data.Word
-- import qualified Network.Socket as S

instance Serialize UTCTime where
  put t = put (unsafeCoerce t :: Int64)
  get = fmap (unsafeCoerce :: Int64 -> UTCTime) get

type HostName    = String
type ServiceName = String
type Addr        = (HostName, ServiceName)

type Message  = String
type Username = String

data ToServer
  = Login String {-- CR-someday: passwords. String --}
  | GetAddr String
  deriving (Typeable, Generic, Show)

data ToClient
  = Friend String (Maybe Addr)
  -- | Friends [(String, Addr)]
  deriving (Typeable, Generic, Show)

data ToPeer
  = Hail Username
  | Message (UTCTime, String)
  deriving (Typeable, Generic)

instance Serialize ToServer where

instance Serialize ToClient where

instance Serialize ToPeer where

-- Delete me:
serverName = "www.izaakmeckler.com"
serverPort = "8080"


{--
newtype PortNumber = PortNum Word16
  deriving (Show, Generic)

instance Serialize PortNumber where


type HostAddress = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)
type FlowInfo = Word32
type ScopeID = Word32


data Addr
  = Inet  PortNumber HostAddress
  | Inet6 PortNumber FlowInfo HostAddress6 ScopeID
  | Unix String
  deriving (Show, Generic)

instance Serialize Addr where

toAddrString :: 

addrFamily :: Addr -> S.Family
addrFamily (Inet _ _)      = S.AF_INET
addrFamily (Inet6 _ _ _ _) = S.AF_INET6
addrFamily (Unix _)        = S.AF_UNIX

toSockAddr :: Addr -> S.SockAddr
toSockAddr (Inet (PortNum p) addr)        = S.SockAddrInet (S.PortNum p) addr
toSockAddr (Inet6 (PortNum p) f addr sid) = S.SockAddrInet6 (S.PortNum p) f addr sid
toSockAddr (Unix s)                       = S.SockAddrUnix s

fromSockAddr :: S.SockAddr -> Addr
fromSockAddr (S.SockAddrInet (S.PortNum p) addr)        = Inet (PortNum p) addr
fromSockAddr (S.SockAddrInet6 (S.PortNum p) f addr sid) = Inet6 (PortNum p) f addr sid
fromSockAddr (S.SockAddrUnix s)                         = Unix s
--}
