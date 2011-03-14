-- | The 'Host' type, and relevant functions
--
module SupHost.Host
    ( Host (..)
    , isAwake
    , wake
    , loadHostFile
    ) where

import Control.Applicative ((<$>))
import System (ExitCode (ExitSuccess), system)

import Network.WoL (sendWoLMagicPacket)

-- | A host
--
data Host = Host
    { hostName :: String
    , hostMac  :: String
    } deriving (Eq, Ord, Show)

-- | Check if a certain host is up.
--
isAwake :: Host -> IO Bool
isAwake host = do
    code <- system $ "ping -c 1 " ++ hostName host ++ " >/dev/null 2>&1"
    return $ code == ExitSuccess

-- | Wake a host
--
wake :: Host -> IO ()
wake host = sendWoLMagicPacket "255.255.255.255" (hostMac host) 17

-- | Parse a host list from a file
--
loadHostFile :: FilePath -> IO [Host]
loadHostFile filePath = concatMap parseHost . lines <$> readFile filePath
  where
    parseHost line = case words line of
        [host, mac] -> [Host host mac]
        _           -> []
