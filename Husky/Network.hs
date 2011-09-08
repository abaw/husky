module Husky.Network
       (
         allHostAddrs
       )
       where

import Network.Info
import Control.Monad


-- | return all IPv4 address of local computer
allHostAddrs :: IO [String]
allHostAddrs = (map show . filter notAllZero . map ipv4)  `liftM` getNetworkInterfaces
               where
                 notAllZero (IPv4 w) = w /= 0
