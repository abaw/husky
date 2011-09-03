import System.Environment
import System.Exit
import System.Directory
import Control.Monad

import Happstack.Server (nullConf, simpleHTTP,ok)
import Happstack.Server.FileServe.BuildingBlocks
import Happstack.Server.Monads

main =  getArgs >>= parseArgs

usage :: IO ()
usage = getProgName >>= \prog ->
  putStrLn ("Usage:" ++ prog ++ " <file>")

parseArgs :: [String] -> IO ()
parseArgs [file] = doesFileExist file >>= mayHostFile file
parseArgs _ = usage >> exitFailure

mayHostFile :: FilePath -> Bool -> IO ()
mayHostFile file True =  do
  putStrLn ("will host " ++ file)
  simpleHTTP nullConf $ do
    setHeaderM "Content-Type" (join $ guessContentTypeM mimeTypes file)
    setHeaderM "Content-Disposition" "attachment; filename=\"test\""
    ok "test"
mayHostFile file False = putStrLn (file ++ " does not exists.")
