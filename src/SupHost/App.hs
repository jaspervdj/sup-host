-- | Webapp
--
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Main
    ( main
    ) where

import Control.Applicative (Alternative, Applicative, (<$>))
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import System.Environment (getProgName, getArgs)
import Data.Maybe (listToMaybe)

import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Char8 as SBC
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config (ConfigListen (..), addListen, emptyConfig)
import Snap.Types ( MonadSnap (..), Snap, route, ifTop, getParam
                  , modifyResponse, addHeader, writeLBS, redirect
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

index :: App ()
index = do
    hosts <- ask
    awake <- mapM (liftIO . isAwake) hosts
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml $ indexView $ zip hosts awake

wakeHost :: App ()
wakeHost = do
    -- Find the host in the list
    name <- fmap SBC.unpack <$> getParam "host"
    hosts <- ask
    let maybeHost = listToMaybe $ filter ((== name) . Just . hostName) hosts

    -- With the host...
    case maybeHost of
        Just host -> liftIO $ wake host
        Nothing   -> return ()

    redirect "../" 

site :: App ()
site = route
    [ ("", ifTop index)
    , ("/:host/wake", wakeHost)
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
