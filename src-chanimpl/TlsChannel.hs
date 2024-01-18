{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module TlsChannel
  ( M
  , TransportException (..)
  , SendException
  , ReceiveException
  , showsPrecSendException
  , showsPrecReceiveException
  , Resource
  , NetworkException (..)
  , send
  , receive
  , tryTls
  ) where

import Control.Exception (Exception, IOException, try)
import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks)
import Foreign.C.Error (Errno)

import Data.ByteString.Lazy qualified as LBS
import Data.Bytes qualified as Bytes
import Data.Bytes.Chunks qualified as Chunks
import Foreign.C.Error.Describe qualified as Describe
import Network.TLS qualified as Tls

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
  showsPrec d (Network e) =
    showParen
      (d > 10)
      (showString "Network " . showsPrecErrno 11 e)
  showsPrec d (System e) =
    showParen
      (d > 10)
      (showString "System " . showsPrec 11 e)
  showsPrec d (TlsException e) =
    showParen
      (d > 10)
      (showString "TlsException " . showsPrec 11 e)

data NetworkException = NetworkException !Errno
  deriving anyclass (Exception)

instance Show NetworkException where
  show (NetworkException e) = Describe.string e

{- | There are three types of exceptions that we can get when
sending/receiving data, so we nest the call to sendData in three
try statements to catch all the possible exceptions.
-}
send ::
  Tls.Context ->
  Chunks ->
  IO (Either TransportException ())
send ctx ch =
  tryTls $ Tls.sendData ctx (LBS.fromStrict (Chunks.concatByteString ch))

receive ::
  Tls.Context ->
  M (Either TransportException Bytes)
receive a =
  tryTls (Tls.recvData a) >>= \case
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
