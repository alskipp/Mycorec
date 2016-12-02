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


data ScientificName a = ScientificName 
    { genus :: a
    , species :: a
    , infraspecific :: Maybe (Infraspecific a)
    , sensu :: Maybe Sensu
    } deriving (Show, Eq, Ord, Functor)


data SourceRecord a = SourceRecord { scientificName :: ScientificName a
                                   , commonNames :: [a]
                                   } deriving (Show, Eq, Ord, Functor)

