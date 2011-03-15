-- | HTML views
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SupHost.Views
    ( indexView
    , hostView
    ) where

import Control.Monad (forM_, unless)
import Data.Monoid (mempty)
import Data.Char (isAlphaNum)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import SupHost.Host (Host (..))

toId :: Host -> String
toId = filter isAlphaNum . hostName

indexView :: [Host] -> Html
indexView hosts = docTypeHtml $ do
    H.head $ do
        H.title "Sup"
        script ! type_ "text/javascript" ! src "jquery-1.5.1.min.js" $ mempty
        script ! type_ "text/javascript" ! src "sup-host.js" $ mempty
    body $ do
        h1 $ "Sup"
        ul $ forM_ hosts $ \host-> li ! A.id (toValue $ toId host) $ do
            "Checking..."
            script ! type_ "text/javascript" $ toHtml $
                "showHost('" ++ hostName host ++ "');"

hostView :: Host -> Bool -> Html
hostView host awake = do
    b $ toHtml name'
    ": "
    if awake then "Up and running!"
             else "Asleep"
    unless awake $
        H.form ! onsubmit onsubmit' $ do
            input ! type_ "submit" ! value "Wake"
  where
    name' = hostName host
    onsubmit' = toValue $ "return wakeHost('" ++ name' ++ "');"
