module Husky.Wai.FileApplication
       (
         fileApplication
       ) where


import System.FilePath
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Enumerator (yield, Stream(EOF))
import Data.CaseInsensitive  ( mk )
import Network.Wai
import Network.HTTP.Types


header :: String -> String -> Header
header key value = (mk (B.pack key), (B.pack value))

-- | WAI application to host file at given urlPath
fileApplication :: String -> FilePath -> Application
fileApplication urlPath file request = do
  liftIO $ putStrLn $ (show $ remoteHost request) ++ " connected."
  if tail (B.unpack $ rawPathInfo request) == urlPath
    then (liftIO . putStrLn)  "good URL." >>
         yield (ResponseFile statusOK headers file Nothing) EOF
    else (liftIO . putStrLn) "bad URL." >>
         yield (responseLBS statusNotFound [headerContentType (B.pack "text/plain")] (LB.pack "404 Not Found")) EOF
  where
    headers = [ headerContentType (B.pack $ guessContentType file) ]

guessContentType :: FilePath -> String
guessContentType path = byExt (takeExtension path)
                        where
                          byExt ".org" = "text/plain"
                          byExt ".txt" = "text/plain"
                          byExt _ = "application/octet-stream"
