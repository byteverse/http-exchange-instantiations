{-# LANGUAGE LambdaCase #-}

{- | Issue insecure HTTP requests using the 'Socket' type from the @network@
library.
-}
module Http.Exchange.Network
  ( -- * Issue Requests
    exchange
  , exchangeInterruptible
  , exchangeTimeout

    -- * Example Use
    -- $example
    -- $exceptionnotes
  , Exception (..)
  , HttpException (..)
  ) where

import Control.Concurrent.STM (TVar, registerDelay)
import Data.Bifunctor (first)
import Http.Types (Bodied, Request, Response)
import Network.Socket (Socket)
import SocketExchange (Exception (..), HttpException (..))

import SocketExchange qualified as X
import SocketInterruptibleChannel qualified as YChan
import SocketInterruptibleExchange qualified as Y

{- | Issue an HTTP request and await a response. This is does not use TLS
(i.e. HTTP, not HTTPS). This function returns exceptions in @Left@ rather
than throwing them, so it is not necessary to use @catch@ when calling it.
-}
exchange ::
  -- | Network socket (TCP or Unix-Domain)
  Socket ->
  -- | HTTP Request
  Bodied Request ->
  -- | HTTP Response or exception
  IO (Either Exception (Bodied Response))
exchange = X.exchange

{- | Variant of exchange that abandons the attempt if the interrupt
variable is set to @True@. If the operation is interrupted in this
way, the result is @EAGAIN@ wrapped by either @Send@ or @Receive@.
See the implementation of 'exchangeTimeout' for an example of how to
use this function to timeout if the HTTP exchange does not complete
quickly.
-}
exchangeInterruptible ::
  -- | Interrupt
  TVar Bool ->
  -- | Network socket (TCP or Unix-Domain)
  Socket ->
  -- | HTTP Request
  Bodied Request ->
  -- | HTTP Response or exception
  IO (Either Exception (Bodied Response))
exchangeInterruptible !a b c =
  fmap (first convertException) (Y.exchange (YChan.Resource b a) c)

{- | Variant of 'exchange' that abandons the exchange if it has not
completed in a given number of microseconds.
-}
exchangeTimeout ::
  -- | Microseconds to wait before giving up
  Int ->
  -- | Network socket (TCP or Unix-Domain)
  Socket ->
  -- | HTTP Request
  Bodied Request ->
  -- | HTTP Response or exception
  IO (Either Exception (Bodied Response))
exchangeTimeout !t sock req = do
  interrupt <- registerDelay t
  exchangeInterruptible interrupt sock req

-- It would be better to fix this problem by changing the structure
-- of http-exchange. The exceptions types are nominally different,
-- but they are isomorphic.
convertException :: Y.Exception -> X.Exception
convertException = \case
  Y.Http x -> X.Http x
  Y.Send x -> X.Send x
  Y.Receive x -> X.Receive x

{- $example

Here is an example that illustrates:

* Resolving a domain name
* Establishing a TCP connection
* Sending and HTTP request and receiving a response

This example issues a request to @http://ifconfig.me/ip@.
It uses functions like @connect@ and @openSocket@ that can throw
exceptions of type @IOException@. It also uses @throwIO@ to throw
any HTTP-related failures, but it is possible to handle these cases
more gracefully. This example requires the @OverloadedStrings@ extension
to be enabled. It is available in @app-http-insecure/Main.hs@.

> communicateWithServer :: IO ()
> communicateWithServer = do
>   let hints = N.defaultHints { N.addrSocketType = N.Stream }
>   minfo <- N.getAddrInfo (Just hints) (Just "ifconfig.me") (Just "80")
>   info <- case minfo of
>     info : _ -> pure info
>     [] -> fail "Impossible: getAddrInfo cannot return empty list"
>   bracket (N.openSocket info) N.close $ \sock -> do
>     N.connect sock (N.addrAddress info)
>     result <- exchange sock Bodied
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
>       Right resp -> pPrint resp -- Variant of print from pretty-show library

Running this results in this being printed:

> Bodied
>   { metadata =
>       Response
>         { statusLine =
>             StatusLine
>               { statusCode = 200
>               , statusReason = "OK"
>               }
>         , headers =
>             [ Header { name = "access-control-allow-origin" , value = "*" }
>             , Header { name = "content-type" , value = "text/plain; charset=utf-8" }
>             , Header { name = "content-length" , value = "15" }
>             , Header { name = "date" , value = "Tue, 15 Aug 2023 16:42:06 GMT" }
>             , Header { name = "x-envoy-upstream-service-time" , value = "1" }
>             , Header
>                 { name = "strict-transport-security"
>                 , value = "max-age=2592000; includeSubDomains"
>                 }
>             , Header { name = "server" , value = "istio-envoy" }
>             , Header { name = "Via" , value = "1.1 google" }
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
