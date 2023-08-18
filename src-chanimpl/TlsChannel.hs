{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language DeriveAnyClass #-}

module TlsChannel
  ( M
  , TransportException(..)
  , SendException
  , ReceiveException
  , showsPrecSendException
  , showsPrecReceiveException
  , Resource
  , SocketThrowingNetworkException(..)
  , NetworkException(..)
  , send
  , receive
  , tryTls
  ) where

import Data.Bytes (Bytes)
import Data.ByteString (ByteString)
import Data.Bytes.Chunks (Chunks)
import Control.Exception (Exception,IOException,try,throwIO)
import Network.Socket (Socket)
import Foreign.C.Error (Errno)

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
type SendException = TransportException
type ReceiveException = TransportException
data TransportException
  = Network !Errno
  | System !IOException
  | TlsException !Tls.TLSException

showsPrecErrno :: Int -> Errno -> String -> String
showsPrecErrno _ e s = Describe.string e ++ (' ' : s)

showsPrecSendException :: Int -> SendException -> String -> String
showsPrecSendException = showsPrec

showsPrecReceiveException :: Int -> ReceiveException -> String -> String
showsPrecReceiveException = showsPrec

instance Show TransportException where
  showsPrec d (Network e) = showParen (d > 10)
    (showString "Network " . showsPrecErrno 11 e)
  showsPrec d (System e) = showParen (d > 10)
    (showString "System " . showsPrec 11 e)
  showsPrec d (TlsException e) = showParen (d > 10)
    (showString "TlsException " . showsPrec 11 e)

data NetworkException = NetworkException !Errno
  deriving anyclass (Exception)

instance Show NetworkException where
  show (NetworkException e) = Describe.string e

-- | Wraps the Socket type. This has different HasBackend instance that
-- throws NetworkException instead of IOException.
-- Elsewhere, when we call Tls.contextNew to create a TLS context,
-- we must use this type instead of Socket.
newtype SocketThrowingNetworkException
  = SocketThrowingNetworkException Socket

instance Tls.HasBackend SocketThrowingNetworkException where
  initializeBackend _ = pure ()
  getBackend (SocketThrowingNetworkException s) =
    buildBackendThrowingNetworkException s

buildBackendThrowingNetworkException :: Socket -> Tls.Backend
buildBackendThrowingNetworkException !s = Tls.Backend
  { Tls.backendFlush = pure ()
  , Tls.backendClose = N.close s
  , Tls.backendSend = \b -> NBS.send s b >>= \case
      Left e -> throwIO (NetworkException e)
      Right () -> pure ()
  , Tls.backendRecv = \n -> receiveExactly s n >>= \case
      Left e -> throwIO (NetworkException e)
      Right bs -> pure bs
  }

-- Note: this imitates the behavior of the auxiliary function recvAll
-- defined in Network.TLS.Backend. If the peer performs an orderly
-- shutdown without sending enough bytes, this function returns
-- successfully with a number of bytes that is less than expected.
receiveExactly :: Socket -> Int -> IO (Either Errno ByteString)
receiveExactly !s !n0 = go [] n0 where
  go !acc 0 = pure (Right (ByteString.concat (List.reverse acc)))
  go !acc n = NBS.receive s n >>= \case
    Left err -> pure (Left err)
    Right b -> case ByteString.length b of
      0 -> pure (Right (ByteString.concat (List.reverse acc)))
      m -> go (b : acc) (n - m)

-- | There are three types of exceptions that we can get when
-- sending/receiving data, so we nest the call to sendData in three
-- try statements to catch all the possible exceptions.   
send ::
     Tls.Context
  -> Chunks
  -> IO (Either TransportException ())
send ctx ch =
  tryTls $ Tls.sendData ctx (LBS.fromStrict (Chunks.concatByteString ch))

receive ::
     Tls.Context
  -> M (Either TransportException Bytes)
receive a = tryTls (Tls.recvData a) >>= \case
  Left err -> pure (Left err)
  Right b -> pure $! Right $! Bytes.fromByteString b

tryTls :: IO a -> IO (Either TransportException a)
tryTls action = do
  e0 <- try $ try $ try $ action
  case e0 of
    Left (NetworkException err) -> pure (Left (Network err))
    Right e1 -> case e1 of
      Left (err :: Tls.TLSException) -> pure (Left (TlsException err))
      Right e2 -> case e2 of
        Left (err :: IOException) -> pure (Left (System err))
        Right a -> pure $! Right a
