module Main (main) where

import DocDiff
import System.Environment

main :: IO ()
main = do
  fileArgs <- getArgs
  store <- preprocess fileArgs
  stats store
