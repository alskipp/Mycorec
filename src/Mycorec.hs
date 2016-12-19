{-# LANGUAGE OverloadedStrings #-}

module Mycorec
    ( parseArgs
    , runExceptT
    ) where

import SourceRecord
import SourceRecordParser
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO
import System.Environment
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class


parseArgs :: ExceptT String IO ()
parseArgs = do
  args <- lift getArgs
  case args of
    [recordsFile] -> do
      recs <- formatSourceRecords recordsFile `catchE` printErr recordsFile
      lift $ T.putStrLn recs

    _ -> do
      name <- lift getProgName
      lift $ hPutStrLn stderr $ "usage: " ++ name ++
        " requires a file path to a Source Record file"
  where printErr file err = do
          lift $ T.putStrLn $ T.intercalate " " ["Could not parse file:", (T.pack file), "|", (T.pack err)]
          throwE err

formatSourceRecords :: FilePath -> ExceptT String IO Text
formatSourceRecords file = fmap (T.unlines . fmap sourceRecordToCSV) (sourceRecordsFromFile file)

sourceRecordsFromFile :: FilePath -> ExceptT String IO [SourceRecord Text]
sourceRecordsFromFile file = do
  t <- lift $ T.readFile file
  ExceptT $ return (sourceRecordsFromText t)
