import Husky
import System.Environment
import System.Exit

main =  getArgs >>= parseArgs

usage :: IO ()
usage = getProgName >>= \prog ->
  putStrLn ("Usage:" ++ prog ++ " <file>")

parseArgs :: [String] -> IO ()
parseArgs [file] = runHusky $ pathConfig file
parseArgs _ = usage >> exitFailure
