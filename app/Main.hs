module Main where

import Mycorec

main :: IO ()
main = do
  runExceptT parseArgs
  return ()
