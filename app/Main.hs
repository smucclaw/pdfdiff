module Main (main) where

import DocDiff
import System.Environment

main :: IO ()
main = do
  fileArgs <- getArgs
  store <- readfiles fileArgs
  writeFile "/tmp/stuff" $ show store
--  stats store
