{-# language LambdaCase #-}

-- | Issue HTTPS requests using the 'Context' type from the @tls@
-- library.
module Http.Exchange.Tls
  ( -- * Issue Requests
    exchange
    -- * Issue Interruptible Requests 
  , exchangeInterruptible
  , exchangeTimeout
  , interruptibleContextNew
  , interruptibleHandshake
  , exposeInterruptibleContext
    -- * Types
  , InterruptibleContext
  , SocketThrowingNetworkException(..)
  , NetworkException(..)
  , TransportException(..)
    -- * Example Use
    -- $example
    -- * Exceptions
    -- $exceptionnotes
  , Exception(..)
  , HttpException(..)
  ) where

import Network.TLS (Context)
import Http.Types (Request,Bodied,Response)

import TlsChannel (NetworkException(..),tryTls)
import TlsChannel (TransportException(..))
import Control.Exception (IOException,try,throwIO)
import TlsExchange (Exception(..),HttpException(..))
import Network.Socket (Socket)
import Foreign.C.Error (Errno)
import Foreign.C.Error (Errno)
import Data.IORef (IORef,readIORef,writeIORef,newIORef)
import Control.Concurrent.STM (TVar,registerDelay)
import Data.ByteString (ByteString)
import qualified TlsExchange as X
import qualified Data.List as List
import qualified Data.ByteString as ByteString
import qualified Network.Socket as N
import qualified Network.Unexceptional.ByteString as NBS
import qualified Network.TLS as Tls
import qualified Network.Unexceptional.Chunks as NC

-- | Issue an HTTP request and await a response. This is does not use TLS
-- (i.e. HTTP, not HTTPS). This function returns exceptions in @Left@ rather
-- than throwing them, so it is not necessary to use @catch@ when calling it.
exchange ::
     Context -- ^ TLS Context
  -> Bodied Request -- ^ HTTP Request
  -> IO (Either Exception (Bodied Response)) -- ^ HTTP Response or exception
exchange = X.exchange

-- | Variant of exchange that abandons the attempt if the interrupt
-- variable is set to @True@. The design of the @tls@ library complicates
-- this function's signature and use. There is an 'InterruptibleContext'
-- type defined in this module that must be used with this function.
-- It is not possible to use the ordinary @Context@ type from @tls@.
-- Example use:
--
-- > clientParams   <- ... -- elided for brevity
-- > theAddressInfo <- ... -- elided for brevity
-- > sock <- ...           -- elided for brevity
-- > N.connect sock theAddressInfo
-- > ctx <- interruptibleContextNew sock clientParams
-- > Tls.handshake ctx
-- > interrupt <- registerDelay 1_000_000
-- > result <- exchange interrupt ctx Bodied{..} -- request body elided
exchangeInterruptible ::
     TVar Bool -- ^ Interrupt
  -> InterruptibleContext -- ^ TLS Context supporting interruption
  -> Bodied Request -- ^ HTTP Request
  -> IO (Either Exception (Bodied Response)) -- ^ HTTP Response or exception
exchangeInterruptible !intr (InterruptibleContext ctx intrRef) !req = do
  writeIORef intrRef intr
  r <- X.exchange ctx req
  writeIORef intrRef interruptibleContextError
  pure r

-- | TLS handshake that can be interrupted. Unlike the original handshake
-- from the @tls@ library, this returns exceptions rather than throwing them.
-- This function must be called before performing any HTTP exchanges on
-- the interruptible context.
interruptibleHandshake ::
     TVar Bool -- ^ Interrupt
  -> InterruptibleContext -- ^ TLS Context supporting interruption
  -> IO (Either TransportException ())
interruptibleHandshake !intr (InterruptibleContext ctx intrRef) = do
  writeIORef intrRef intr
  x <- tryTls (Tls.handshake ctx)
  writeIORef intrRef interruptibleContextError
  pure x

-- | Variant of 'exchange' that abandons the exchange if it has not
-- completed in a given number of microseconds.
exchangeTimeout :: 
     Int -- ^ Microseconds to wait before giving up
  -> InterruptibleContext -- ^ TLS Context supporting interruption
  -> Bodied Request -- ^ HTTP Request
  -> IO (Either Exception (Bodied Response)) -- ^ HTTP Response or exception
exchangeTimeout !t ctx req = do
  interrupt <- registerDelay t
  exchangeInterruptible interrupt ctx req

{- $example

Here is an example that illustrates:

* Resolving a domain name
* Establishing a TCP connection
* Sending and HTTP request and receiving a response

This example issues a request to @https://ifconfig.me/ip@.
It uses functions like @connect@ and @openSocket@ that can throw
exceptions of type @IOException@. It uses @handshake@, which can
throw exceptions of type @TLSException@. It also uses @throwIO@ to throw
any HTTP-related failures, but it is possible to handle these cases
more gracefully. This example requires the @OverloadedStrings@ extension
to be enabled. It is available in @app-http-secure/Main.hs@.

> communicateWithServer :: IO ()
> communicateWithServer = do
>   let noValidation = Tls.ValidationCache
>         (\_ _ _ -> return Tls.ValidationCachePass)
>         (\_ _ _ -> return ())
>   let clientParams = (Tls.defaultParamsClient "ifconfig.me" mempty)
>         { Tls.clientSupported = def
>           { Tls.supportedVersions = [Tls.TLS13]
>           , Tls.supportedCiphers =
>             [ Tls.cipher_TLS13_AES128GCM_SHA256
>             , Tls.cipher_TLS13_AES256GCM_SHA384
>             , Tls.cipher_TLS13_CHACHA20POLY1305_SHA256
>             , Tls.cipher_TLS13_AES128CCM_SHA256
>             , Tls.cipher_TLS13_AES128CCM8_SHA256
>             ]
>           }
>         , Tls.clientShared = def
>           { Tls.sharedValidationCache = noValidation
>           }
>         }
>   let hints = N.defaultHints { N.addrSocketType = N.Stream }
>   minfo <- N.getAddrInfo (Just hints) (Just "ifconfig.me") (Just "443")
>   info <- case minfo of
>     info : _ -> pure info
>     [] -> fail "Impossible: getAddrInfo cannot return empty list"
>   bracket (N.openSocket info) N.close $ \sock -> do
>     N.connect sock (N.addrAddress info)
>     ctx <- Tls.contextNew (SocketThrowingNetworkException sock) clientParams
>     Tls.handshake ctx
>     result <- exchange ctx Bodied
>       { metadata = Request
>         { requestLine = RequestLine
>           { method = "GET"
>           , path = "/ip"
>           }
>         , headers = Headers.fromList
>           [ Header "Host" "ifconfig.me"
>           , Header "Accept" "text/plain"
>           , Header "User-Agent" "curl/0.0.0"
>           ]
>         }
>       , body = mempty
>       }
>     case result of
>       Left e -> throwIO e
>       Right resp -> pPrint resp

Running this results in this being printed:

> Bodied
>   { metadata =
>       Response
>         { statusLine =
>             StatusLine { statusCode = 200 , statusReason = "OK" }
>         , headers =
>             [ Header { name = "access-control-allow-origin" , value = "*" }
>             , Header { name = "content-type" , value = "text/plain; charset=utf-8" }
>             , Header { name = "content-length" , value = "15" }
>             , Header { name = "date" , value = "Tue, 15 Aug 2023 19:57:40 GMT" }
>             , Header { name = "x-envoy-upstream-service-time" , value = "2" }
>             , Header
>                 { name = "strict-transport-security"
>                 , value = "max-age=2592000; includeSubDomains"
>                 }
>             , Header { name = "server" , value = "istio-envoy" }
>             , Header { name = "Via" , value = "1.1 google" }
>             , Header
>                 { name = "Alt-Svc"
>                 , value = "h3=\":443\"; ma=2592000,h3-29=\":443\"; ma=2592000"
>                 }
>             ]
>         }
>   , body = ...
>   }


-}

{- $exceptionnotes

Note: In the documentation generated by Haddock, the @Send@ and @Receive@
data constructors in the @Exception@ type show types from an indefinite
module, but in this instantiation, these types are both aliases
for 'Foreign.C.Error.Errno'.
-}

-- | Wraps the Socket type. This has different HasBackend instance that
-- throws NetworkException instead of IOException.
-- Elsewhere, when we call Tls.contextNew to create a TLS context,
-- we must use this type instead of Socket.
newtype SocketThrowingNetworkException
  = SocketThrowingNetworkException Socket

data InterruptibleContext
  = InterruptibleContext !Tls.Context !(IORef (TVar Bool))

interruptibleContextError :: TVar Bool
{-# noinline interruptibleContextError #-}
interruptibleContextError =
  errorWithoutStackTrace "Http.Exchange.Tls: misuse of InterruptibleContext"

-- | Create a new TLS context that supports interrupting exchanges
-- with a 'TVar'.
interruptibleContextNew :: (Tls.TLSParams params)
  => Socket -- ^ Network socket. Must already be connected.
  -> params -- ^ Parameters of the context.
  -> IO InterruptibleContext
interruptibleContextNew socket params = do
  !intrRef <- newIORef interruptibleContextError
  let backend = buildInterruptibleBackend socket intrRef
  context <- Tls.contextNew backend params
  pure (InterruptibleContext context intrRef)

-- | Expose the TLS context. Do not call TLS data-exchange functions like
-- @sendData@, @recvData@, or @handshake@ on this context. This context is
-- exposed so that the caller can query it for metadata about the session
-- (certs, etc.).
exposeInterruptibleContext :: InterruptibleContext -> Tls.Context
{-# inline exposeInterruptibleContext #-}
exposeInterruptibleContext (InterruptibleContext c _) = c

buildInterruptibleBackend :: Socket -> IORef (TVar Bool) -> Tls.Backend
buildInterruptibleBackend s !intrRef = Tls.Backend
  { Tls.backendFlush = pure ()
  , Tls.backendClose = N.close s
  , Tls.backendSend = \b -> do
      !interrupt <- readIORef intrRef
      NBS.sendInterruptible interrupt s b >>= \case
        Left e -> throwIO (NetworkException e)
        Right () -> pure ()
  , Tls.backendRecv = \n -> do
      !interrupt <- readIORef intrRef
      NBS.receiveExactlyInterruptible interrupt s n >>= \case
        Left e -> throwIO (NetworkException e)
        Right bs -> pure bs
  }
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
  -- Note: This receive function does not imitate the behavior of the
  -- auxiliary function recvAll defined in Network.TLS.Backend. If the
  -- peer performs an orderly shutdown without sending enough bytes,
  -- this throws EEOI.
  , Tls.backendRecv = \n -> NBS.receiveExactly s n >>= \case
      Left e -> throwIO (NetworkException e)
      Right bs -> pure bs
  }
