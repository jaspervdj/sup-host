-- | Webapp
--
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Main
    ( main
    ) where

import Control.Applicative (Alternative, Applicative, (<$>), (<|>))
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import System.Environment (getProgName, getArgs)
import Data.Maybe (listToMaybe)

import Text.Blaze (Html)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Char8 as SBC
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config (ConfigListen (..), addListen, emptyConfig)
import Snap.Util.FileServe (serveDirectory)
import Snap.Types ( MonadSnap (..), Snap, route, ifTop, getParam
                  , modifyResponse, addHeader, writeLBS
                  )

import SupHost.Host
import SupHost.Views

-- | Application monad stack
--
newtype App a = App
    { unApp :: ReaderT [Host] Snap a
    } deriving ( Functor, Applicative, Alternative, Monad, MonadReader [Host]
               , MonadIO, MonadPlus, MonadCatchIO
               )

instance MonadSnap App where
    liftSnap = App . lift

respondBlaze :: Html -> App ()
respondBlaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

index :: App ()
index = do
    hosts <- ask
    respondBlaze $ indexView hosts

withHost :: (Host -> App ()) -> App ()
withHost f = do
    -- Find the host in the list
    name <- fmap SBC.unpack <$> getParam "host"
    hosts <- ask
    let maybeHost = listToMaybe $ filter ((== name) . Just . hostName) hosts

    -- With the host...
    case maybeHost of
        Just host -> f host
        Nothing   -> return ()

wakeHost :: App ()
wakeHost = do
    withHost $ liftIO . wake
    respondBlaze "WoL signal sent"

showHost :: App ()
showHost = withHost $ \host -> do
    awake <- liftIO $ isAwake host
    liftIO $ putStrLn $ "Checked: " ++ show host
    respondBlaze $ hostView host awake

site :: App ()
site = serveDirectory "static" <|> route
    [ ("", ifTop index)
    , ("/:host/wake", wakeHost)
    , ("/:host", showHost)
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- Enough arguments given
        [port, hostFile] -> do
            hosts <- loadHostFile hostFile
            httpServe (config $ read port) (runReaderT (unApp site) hosts)
        -- More aguments please
        _                -> do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " <port> <host file>"
  where
    config port = addListen (ListenHttp "0.0.0.0" port)
                $ emptyConfig
