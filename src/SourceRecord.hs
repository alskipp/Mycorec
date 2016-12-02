{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module SourceRecord where
    
import Data.Text (Text)
import qualified Data.Text as T hiding (toTitle)
import qualified TextToTitle as T


data Infraspecific a = SubSpecies a -- subsp. or ssp.
                     | Variety a    -- var.
                     | SubVariety a -- subvar.
                     | Forma a      -- f.
                     | SubForma a   -- subf.
                     deriving (Show, Eq, Ord, Functor)


data Sensu = Stricto -- s.s
           | Lato    -- s.l
           deriving (Show, Eq, Ord)


data ScientificName a = ScientificName 
    { genus :: a
    , species :: a
    , infraspecific :: Maybe (Infraspecific a)
    , sensu :: Maybe Sensu
    } deriving (Show, Eq, Ord, Functor)


data SourceRecord a = SourceRecord { scientificName :: ScientificName a
                                   , commonNames :: [a]
                                   } deriving (Show, Eq, Ord, Functor)



sourceRecordToCSV :: SourceRecord Text -> Text
sourceRecordToCSV r = T.intercalate "," [ppScientificName (scientificName r), ppCommonNames]
  where ppCommonNames = T.intercalate " / " $ T.toTitle <$> (commonNames r)


ppScientificName :: ScientificName Text -> Text
ppScientificName rec = T.concat [T.toTitle (genus rec), " ", species rec, infraT (infraspecific rec), sensuT (sensu rec)]
  where infraT = maybe "" (T.append " " . ppInfraspecific)
        sensuT = maybe "" (T.append " " . ppSensu)

ppInfraspecific :: Infraspecific Text -> Text
ppInfraspecific (SubSpecies s) = T.append "subsp. " s
ppInfraspecific (Variety s)    = T.append "var. " s
ppInfraspecific (SubVariety s) = T.append "subvar. " s
ppInfraspecific (Forma s)      = T.append "f. " s
ppInfraspecific (SubForma s)   = T.append  "subf. " s

ppSensu :: Sensu -> Text
ppSensu Stricto = "s.s"
ppSensu Lato    = "s.l"
