-- | HTML views
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SupHost.Views
    ( index
    , wakeHost
    , showHost
    ) where

import Control.Monad (forM_, unless)
import Data.Monoid (mempty)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import SupHost.Host (Host (..))

index :: [Host] -> String -> Html
index hosts time' = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
        H.title "Sup Host"
        script ! type_ "text/javascript" ! src "jquery-1.5.1.min.js" $ mempty
        script ! type_ "text/javascript" ! src "sup-host.js" $ mempty
        link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
        h1 $ "Sup Host"
        ul $ forM_ hosts $ \host-> li ! attr host $ do
            H.span ! class_ "host-name" $ toHtml $ hostName host
            ": "
            H.span ! class_ "host-content" $
                img ! src "loader.gif" ! alt "Loading..."
            script ! type_ "text/javascript" $ toHtml $
                "showHost('" ++ hostName host ++ "');"
        footer $ do
            p $ "Last updated " >> toHtml time'
            a ! href "http://github.com/jaspervdj/sup-host" $ "source code"
  where
    attr = dataAttribute "host" . toValue . hostName

wakeHost :: Html
wakeHost = "WoL signal sent"

showHost :: Host -> Bool -> Html
showHost host awake = do
    if awake then "Up and running!"
             else "Asleep"
    unless awake $
        H.form ! onsubmit onsubmit' $ do
            input ! type_ "submit" ! value "Wake"
  where
    name' = hostName host
    onsubmit' = toValue $ "return wakeHost('" ++ name' ++ "');"
