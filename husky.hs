import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Random (newStdGen, randomRs)
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Enumerator (yield, Stream(EOF))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Rewrite (rewrite)
import Network.HTTP.Types
import Data.CaseInsensitive  ( mk )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T

import qualified Network.Socket as S

main =  getArgs >>= parseArgs

usage :: IO ()
usage = getProgName >>= \prog ->
  putStrLn ("Usage:" ++ prog ++ " <file>")

parseArgs :: [String] -> IO ()
parseArgs [file] = doesFileExist file >>= mayHostFile file
parseArgs _ = usage >> exitFailure

-- | return a random string with the given length
randomString :: Int -> IO String
randomString len = do
  gen <- newStdGen
  return $ map (elemAt validChars) $ take len $ randomChars gen
  where
    randomChars gen = randomRs (0, (length validChars)-1) gen
    validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_','-','~','/']
    elemAt list idx = head $ snd $ splitAt idx list

-- | return a tcp socket bound on any port
anyTCPSocketBound :: IO S.Socket
anyTCPSocketBound =
  do s <- S.socket S.AF_INET S.Stream S.defaultProtocol
     S.bindSocket s (S.SockAddrInet S.aNY_PORT S.iNADDR_ANY)
     S.listen s 5
     return s

mayHostFile :: FilePath -> Bool -> IO ()
mayHostFile file True =  do
  leadingPath <- randomString 5
  let urlPath = leadingPath ++ "/" ++ (takeFileName file)
  s <- anyTCPSocketBound
  port <- (S.socketPort s)
  -- putStrLn ("http://localhost:3000/" ++ urlPath)
  putStrLn $ "http://localhost:" ++ (show port) ++ "/" ++ urlPath
  --run 3000 $ fileApplication urlPath file
  runSettingsSocket defaultSettings s $ fileApplication urlPath file
mayHostFile file False = putStrLn (file ++ " does not exists.")

header :: String -> String -> Header
header key value = (mk (B.pack key), (B.pack value))

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
