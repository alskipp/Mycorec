module Mycorec
    ( parseArgs
    ) where

import SourceRecord
import SourceRecordParser
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO
import System.Environment
import System.Exit


parseArgs :: IO ()
parseArgs = do
  args <- getArgs
  case args of 
    [recordsFile] -> do
      recs <- formatSourceRecords recordsFile
      T.putStr recs
      
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ 
        " requires a file path to a Source Record file"
      exitFailure
      
formatSourceRecords :: FilePath -> IO Text
formatSourceRecords file = fmap T.unlines $ fmap (fmap sourceRecordToCSV) (sourceRecordsFromFile file)
    
sourceRecordsFromFile :: FilePath -> IO [SourceRecord Text]
sourceRecordsFromFile file = do
  recordStr <- T.readFile file
  case sourceRecordsFromText recordStr of
    (Left _)  -> return []
    (Right r) -> return r
