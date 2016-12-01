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


recordsFromText :: Text -> Either String [SourceRecord]
recordsFromText = parseOnly parseAllSourceRecords

parseAllSourceRecords :: Parser [SourceRecord]
parseAllSourceRecords = many (parseSourceRecord <* many endOfLine)


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
parseCommonNames  = names <$> notSpaces `sepBy1` takeWhile isHorizontalSpace
  where names     = fmap T.unwords . (splitWhen (=="/"))
        notSpaces = takeWhile1 (not . isSpace)


parseScientificName :: Parser ScientificName
parseScientificName = ScientificName 
  <$> spacesThenName
  <*> parseSpecies
  <*> (option Nothing (fmap Just parseInfraspecific))
  <*> (option Nothing (fmap Just parseSensu))


parseSourceRecord :: Parser SourceRecord
parseSourceRecord = SourceRecord 
  <$> parseScientificName
  <*> (stripSpaces (char ',') *> parseCommonNames)


stripSpaces p = skipSpace *> p <* skipSpace

parseName, spacesThenName :: Parser Text
parseName      = takeWhile1 $ isLetter `or` (== '-') `or` (== '\'') `or` (== '’')
  where or = liftA2 (||)

spacesThenName = skipSpace *> parseName
