{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket, throwIO)
import Http.Exchange.Network (exchange)
import Http.Headers qualified as Headers
import Http.Types (Bodied (..), Header (Header), Request (..), RequestLine (..))
import Network.Socket qualified as N
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  minfo <- N.getAddrInfo (Just hints) (Just "ifconfig.me") (Just "80")
  info <- case minfo of
    info : _ -> pure info
    [] -> fail "Impossible: getAddrInfo cannot return empty list"
  bracket (N.openSocket info) N.close $ \sock -> do
    N.connect sock (N.addrAddress info)
    result <-
      exchange
        sock
        Bodied
          { metadata =
              Request
                { requestLine =
                    RequestLine
                      { method = "GET"
                      , path = "/ip"
                      }
                , headers =
                    Headers.fromList
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
