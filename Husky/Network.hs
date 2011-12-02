module Husky.Network
       (
         allHostAddrs
       )
       where

import Network.Info
import Control.Monad
import Data.Bits
import Data.Binary
import Data.Binary.Put


-- | return all IPv4 address of local computer
allHostAddrs :: IO [String]
allHostAddrs = (map show . filter wanted . map ipv4)  `liftM` getNetworkInterfaces
               where
                 wanted (IPv4 w) = w /= 0 && w/= loopback
		 loopback = decode $ runPut $ putWord32host $ shift 127 24 + 1
