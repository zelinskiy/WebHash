{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib (startApp) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad
import Control.Monad.IO.Class
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as BS

import Hash

type API = "static" :> Raw
  :<|> "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] String

startApp :: IO ()
startApp = run 8080 app
  where app = serve (Proxy :: Proxy API) server

server :: Server API
server = serveDirectoryFileServer "./static"
  :<|> upload

upload :: MultipartData Mem -> Handler String
upload multipartData = do
  let [file] = files multipartData
  res <- liftIO $ testHashes $ fdPayload file
  liftIO $ putStr "Received: "
  liftIO $ print $ fdFileName file
  return res


