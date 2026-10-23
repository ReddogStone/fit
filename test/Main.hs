module Main (main) where

import Control.Monad (forM)
import Data.List (isSuffixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Format (format)
import Parse (parseFile)
import Run
import System.Directory (doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, (</>))

main :: IO ()
main = do
  files <- concat <$> mapM listCore ["examples", "examples" </> "errors"]
  loaded <- mapM (\path -> T.readFile path >>= either fail pure . parseFile path) ["host.core", "prelude.core"]
  results <- forM files $ \path -> do
    source <- T.readFile path
    expected <- do
      let outPath = replaceExtension path "out"
      exists <- doesFileExist outPath
      if exists then Just <$> readFile outPath else pure Nothing
    let execute text = case parseFile path text of
          Left err -> err
          Right entry -> run entry loaded ++ "\n"
        actual = execute source
        formatted = format path source
        stable = formatted == (format path . T.pack =<< formatted)
        sameOutput = (execute . T.pack <$> formatted) == Right actual
        ok = Just actual == expected && stable && sameOutput
    putStrLn ((if ok then "  ok   " else "  FAIL ") ++ path)
    if ok then pure () else putStr actual >> either putStrLn (const (pure ())) formatted
    pure ok
  let failed = length (filter not results)
  putStrLn (show (length results - failed) ++ " files passed, " ++ show failed ++ " failed")
  if failed > 0 then exitFailure else pure ()

listCore :: FilePath -> IO [FilePath]
listCore dir = map (dir </>) . sort . filter (".core" `isSuffixOf`) <$> listDirectory dir
