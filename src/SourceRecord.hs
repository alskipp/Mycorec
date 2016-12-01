{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module SourceRecord where
    
import Data.Text (Text)
import qualified Data.Text as T


data Infraspecific a = SubSpecies a -- subsp. or ssp.
                     | Variety a    -- var.
                     | SubVariety a -- subvar.
                     | Forma a      -- f.
                     | SubForma a   -- subf.
                     deriving (Show, Eq, Ord, Functor)


data Sensu = Stricto -- s.s
           | Lato    -- s.l
           deriving (Show, Eq, Ord)


data ScientificName = ScientificName 
    { genus :: Text
    , species :: Text
    , infraspecific :: Maybe (Infraspecific Text)
    , sensu :: Maybe Sensu
    } deriving (Show, Eq, Ord)


data SourceRecord = SourceRecord { scientificName :: ScientificName
                                 , commonNames :: [Text]
                                 } deriving (Show, Eq, Ord)
