module SocketChannel
  ( M
  , SendException
  , ReceiveException
  , showsPrecSendException
  , showsPrecReceiveException
  , Resource
  , send
  , receive
  ) where

import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks)
import Foreign.C.Error (Errno)
import Network.Socket (Socket)

import Foreign.C.Error.Describe qualified as Describe
import Network.Unexceptional.Bytes qualified as NB
import Network.Unexceptional.Chunks qualified as NC

type M = IO

type Resource = Socket
type ReceiveException = Errno
type SendException = Errno

showsPrecSendException :: Int -> SendException -> String -> String
showsPrecSendException = showsPrecErrno

showsPrecReceiveException :: Int -> ReceiveException -> String -> String
showsPrecReceiveException = showsPrecErrno

showsPrecErrno :: Int -> Errno -> String -> String
showsPrecErrno _ e s = Describe.string e ++ (' ' : s)

send ::
  Resource ->
  Chunks ->
  M (Either Errno ())
send = NC.send

receive ::
  Resource ->
  M (Either Errno Bytes)
receive a = NB.receive a 12000
