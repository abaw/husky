module Husky
       (
         runHusky
       , pathConfig
       )
       where

import System.Directory
import System.FilePath
import System.Random (newStdGen, randomRs)

import Control.Monad (liftM)

import Network.Wai.Handler.Warp
import Network.Socket

import Husky.Wai.FileApplication
import Husky.Network


data Config = Config
              { path :: FilePath
              , debug :: Bool
              }

-- | return a random string with the given length
randomString :: Int -> IO String
randomString len = do
  gen <- newStdGen
  return $ map (validChars !!) $ take len $ randomChars gen
  where
    randomChars gen = randomRs (0, (length validChars)-1) gen
    validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_','-','~','/']

-- | return a tcp socket bound on any port
anyTCPSocketBound :: IO Socket
anyTCPSocketBound =
  do s <- socket AF_INET Stream defaultProtocol
     bindSocket s (SockAddrInet aNY_PORT iNADDR_ANY)
     listen s 5
     return s


mayHostFile :: FilePath -> Bool -> IO ()
mayHostFile file True =  do
  leadingPath <- randomString 5
  let urlPath = leadingPath ++ "/" ++ (takeFileName file)
  s <- anyTCPSocketBound
  port <- (socketPort s)
  urls <- map (formURL port urlPath) `liftM` allHostAddrs
  putStrLn $ "host file \"" ++ file ++ "\" on these URLs:"
  mapM_ putStrLn urls
  runSettingsSocket defaultSettings s $ fileApplication urlPath file
  where
    formURL port path host = "http://" ++ host ++ ":" ++ (show port) ++ "/" ++ path
mayHostFile file False = putStrLn (file ++ " does not exists.")

runHusky :: Config -> IO ()
runHusky conf = doesFileExist file >>= mayHostFile file
                where
                  file = path conf

pathConfig :: FilePath -> Config
pathConfig path = defaultConfig { path = path }

defaultConfig :: Config
defaultConfig = Config { path = "", debug = False }
