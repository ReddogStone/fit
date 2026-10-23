module Main (main) where

import qualified Data.Text.IO as T
import Format (format)
import Parse (parseFile)
import Run
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  getArgs >>= \case
    ["fmt", path] -> T.readFile path >>= emit . format path
    entry : loaded | entry /= "fmt" -> do
      files <- mapM (\path -> (path,) <$> T.readFile path) (entry : loaded)
      emit (mapM (uncurry parseFile) files >>= \case
        parsed : rest -> Right (run parsed rest)
        [] -> Left "usage: core FILE [FILE...] | core fmt FILE")
    _ -> failWith "usage: core FILE [FILE...] | core fmt FILE"

emit :: Either String String -> IO ()
emit = either failWith putStrLn

failWith :: String -> IO ()
failWith message = hPutStrLn stderr message >> exitFailure
