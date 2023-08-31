module SocketInterruptibleChannel
  ( M
  , SendException
  , ReceiveException
  , showsPrecSendException
  , showsPrecReceiveException
  , Resource(..)
  , send
  , receive
  ) where

import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks)
import Network.Socket (Socket)
import Foreign.C.Error (Errno)
import Control.Concurrent.STM (TVar)

import qualified Foreign.C.Error.Describe as Describe
import qualified Network.Unexceptional.Bytes as NB
import qualified Network.Unexceptional.Chunks as NC

type M = IO

data Resource = Resource {-# UNPACK #-} !Socket !(TVar Bool)

type ReceiveException = Errno
type SendException = Errno

showsPrecSendException :: Int -> SendException -> String -> String
showsPrecSendException = showsPrecErrno

showsPrecReceiveException :: Int -> ReceiveException -> String -> String
showsPrecReceiveException = showsPrecErrno

showsPrecErrno :: Int -> Errno -> String -> String
showsPrecErrno _ e s = Describe.string e ++ (' ' : s)

send ::
     Resource
  -> Chunks
  -> M (Either Errno ())
send (Resource a interrupt) b = do
  NC.sendInterruptible interrupt a b

receive ::
     Resource
  -> M (Either Errno Bytes)
receive (Resource a interrupt) = NB.receiveInterruptible interrupt a 12000

