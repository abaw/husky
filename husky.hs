import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Random (newStdGen, randomRs)

import Network.Wai.Handler.Warp
import Network.Socket

import Husky.Wai.FileApplication

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
  putStrLn $ "http://localhost:" ++ (show port) ++ "/" ++ urlPath
  runSettingsSocket defaultSettings s $ fileApplication urlPath file
mayHostFile file False = putStrLn (file ++ " does not exists.")


