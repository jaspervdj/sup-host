-- | HTML views
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SupHost.Views
    ( indexView
    ) where

import Control.Monad (forM_, unless)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import SupHost.Host (Host (..))

indexView :: [(Host, Bool)] -> Html
indexView hosts = docTypeHtml $ do
    H.head $ do
        H.title "Sup"
    body $ do
        h1 $ "Sup"
        ul $ forM_ hosts $ \(host, awake) -> li $ do
            let name' = hostName host
                wake = "/" ++ name' ++ "/wake"
            b $ toHtml name'
            ": "
            if awake then "Up and running!"
                     else "Asleep"
            unless awake $
                H.form ! method "POST" ! action (toValue wake) $ do
                    input ! type_ "submit" ! value "Wake"
