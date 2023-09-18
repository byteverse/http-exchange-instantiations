{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language DeriveAnyClass #-}

module TlsInterruptibleChannel
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
import Data.ByteString (ByteString)
import Data.Bytes.Chunks (Chunks)
import Network.Socket (Socket)
import Foreign.C.Error (Errno)
import Control.Concurrent.STM (TVar)
import TlsChannel (TransportException(..),NetworkException(..))
import TlsChannel (showsPrecSendException,showsPrecReceiveException,tryTls)
import Control.Exception (Exception,IOException,try,throwIO)

import qualified Data.Bytes as Bytes
import qualified Data.List as List
import qualified Data.Bytes.Chunks as Chunks
import qualified Network.Socket as N
import qualified Network.Unexceptional.ByteString as NBS
import qualified Network.TLS as Tls
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as ByteString
import qualified Foreign.C.Error.Describe as Describe

type M = IO

type Resource = Tls.Context
data InterruptibleSocket =
  InterruptibleSocket {-# UNPACK #-} !Socket !(TVar Bool)
type SendException = TransportException
type ReceiveException = TransportException

instance Tls.HasBackend InterruptibleSocket where
  initializeBackend _ = pure ()
  getBackend = buildBackendThrowingNetworkException

buildBackendThrowingNetworkException :: Resource -> Tls.Backend
buildBackendThrowingNetworkException (InterruptibleSocket s interrupt) = Tls.Backend
  { Tls.backendFlush = pure ()
  , Tls.backendClose = N.close s
  , Tls.backendSend = \b -> NC.sendInterruptible interrupt s b >>= \case
      Left e -> throwIO (NetworkException e)
      Right () -> pure ()
  , Tls.backendRecv = \n -> receiveExactly interrupt s n >>= \case
      Left e -> throwIO (NetworkException e)
      Right bs -> pure bs
  }

-- Note: this imitates the behavior of the auxiliary function recvAll
-- defined in Network.TLS.Backend. If the peer performs an orderly
-- shutdown without sending enough bytes, this function returns
-- successfully with a number of bytes that is less than expected.
receiveExactly :: TVar Bool -> Socket -> Int -> IO (Either Errno ByteString)
receiveExactly !interrupt !s !n0 = go [] n0 where
  go !acc 0 = pure (Right (ByteString.concat (List.reverse acc)))
  go !acc n = NBS.receiveInterruptible interrupt s n >>= \case
    Left err -> pure (Left err)
    Right b -> case ByteString.length b of
      0 -> pure (Right (ByteString.concat (List.reverse acc)))
      m -> go (b : acc) (n - m)

-- | There are three types of exceptions that we can get when
-- sending/receiving data, so we nest the call to sendData in three
-- try statements to catch all the possible exceptions.   
send ::
     Resource
  -> Chunks
  -> IO (Either TransportException ())
send (Resource ctx interrupt) ch =
  tryTls $ Tls.sendData (InterruptibleSocket ctx interrupt) (LBS.fromStrict (Chunks.concatByteString ch))

receive ::
     Resource
  -> M (Either TransportException Bytes)
receive (Resource ctx interrupt) = tryTls (Tls.recvData (InterruptibleSocket ctx interrupt)) >>= \case
  Left err -> pure (Left err)
  Right b -> pure $! Right $! Bytes.fromByteString b
