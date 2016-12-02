{-# LANGUAGE OverloadedStrings #-}

module SourceRecordParser where


import SourceRecord
import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.List.Split (splitWhen)
import Data.Char


sourceRecordsFromText :: Text -> Either String [SourceRecord Text]
sourceRecordsFromText = parseLines parseSourceRecordToLower

parseLines :: Parser (SourceRecord Text) -> Text -> Either String [SourceRecord Text]
parseLines parser = parseOnly everyLine
  where everyLine = parser `sepBy1` (takeWhile1 isEndOfLine)


parseSpecies :: Parser Text
parseSpecies = spacesThenName


parseSensu :: Parser Sensu
parseSensu = skipSpace *> (
  Stricto <$ "s.s" <|>
  Lato <$ "s.l"
  )


parseInfraspecific :: Parser (Infraspecific Text)
parseInfraspecific =
  Variety    <$> infra "var" <|>
  Forma      <$> infra "f" <|>
  SubSpecies <$> (infra "subsp" <|> infra "ssp") <|>
  SubVariety <$> infra "subvar" <|>
  SubForma   <$> infra "subf"
    where infra str = skipSpace *> str *> takeWhile (== '.') *> spacesThenName


parseCommonNames :: Parser [Text]
parseCommonNames  = names <$> nameOrSep `sepEndBy1` takeWhile isHorizontalSpace
  where names     = fmap T.unwords . (splitWhen (=="/"))
        nameOrSep = parseName <|> "/"


parseScientificName :: Parser (ScientificName Text)
parseScientificName = ScientificName 
  <$> spacesThenName
  <*> parseSpecies
  <*> (option Nothing (fmap Just parseInfraspecific))
  <*> (option Nothing (fmap Just parseSensu))


parseSourceRecord :: Parser (SourceRecord Text)
parseSourceRecord = SourceRecord 
  <$> parseScientificName
  <*> (stripSpaces (char ',') *> parseCommonNames)

parseSourceRecordToLower :: Parser (SourceRecord Text)
parseSourceRecordToLower = fmap (fmap T.toLower) parseSourceRecord
  

stripSpaces p = skipSpace *> p <* skipSpace

parseName, spacesThenName :: Parser Text
parseName = takeWhile1 $ isLetter `or` (== '-') `or` (== '\'') `or` (== '’') `or` (== '.')
  where or = liftA2 (||)

spacesThenName = skipSpace *> parseName

----------------------------------
-- Additional AttoParsec functions

sepEndBy1 :: Parser a -> Parser b -> Parser [a]
sepEndBy1 p sep = do x <- p
                     do _  <- sep
                        xs <- sepEndBy p sep
                        return (x:xs) <|> return [x]

sepEndBy :: Parser a -> Parser b -> Parser [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []
