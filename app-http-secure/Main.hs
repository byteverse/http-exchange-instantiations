{-# language OverloadedStrings #-}

import Control.Exception (bracket,throwIO)
import Http.Exchange.Tls (SocketThrowingNetworkException(..),exchange)
import Http.Types (Request(..),RequestLine(..),Bodied(..),Header(Header))
import Text.Show.Pretty (pPrint)
import Data.Default (def)
import qualified Http.Headers as Headers
import qualified Network.Socket as N
import qualified Network.TLS.Extra.Cipher as Tls
import qualified Network.TLS as Tls

main :: IO ()
main = do
  let noValidation = Tls.ValidationCache
        (\_ _ _ -> return Tls.ValidationCachePass)
        (\_ _ _ -> return ())
  let clientParams = (Tls.defaultParamsClient "ifconfig.me" mempty)
        { Tls.clientSupported = def
          { Tls.supportedVersions = [Tls.TLS13]
          , Tls.supportedCiphers =
            [ Tls.cipher_TLS13_AES128GCM_SHA256
            , Tls.cipher_TLS13_AES256GCM_SHA384
            , Tls.cipher_TLS13_CHACHA20POLY1305_SHA256
            , Tls.cipher_TLS13_AES128CCM_SHA256
            , Tls.cipher_TLS13_AES128CCM8_SHA256
            ]
          }
        , Tls.clientShared = def
          { Tls.sharedValidationCache = noValidation
          }
        }
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  minfo <- N.getAddrInfo (Just hints) (Just "ifconfig.me") (Just "443")
  info <- case minfo of
    info : _ -> pure info
    [] -> fail "Impossible: getAddrInfo cannot return empty list"
  bracket (N.openSocket info) N.close $ \sock -> do
    N.connect sock (N.addrAddress info)
    ctx <- Tls.contextNew (SocketThrowingNetworkException sock) clientParams
    Tls.handshake ctx
    result <- exchange ctx Bodied
      { metadata = Request
        { requestLine = RequestLine
          { method = "GET"
          , path = "/ip"
          }
        , headers = Headers.fromList
          [ Header "Host" "ifconfig.me"
          , Header "Accept" "text/plain"
          , Header "User-Agent" "curl/0.0.0"
          ]
        }
      , body = mempty
      }
    case result of
      Left e -> throwIO e
      Right resp -> pPrint resp

