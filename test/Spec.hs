{-# LANGUAGE OverloadedStrings #-}

module Main where

import Mycorec
import SourceRecord
import SourceRecordParser
import Test.Hspec
import Data.Either


main :: IO ()
main = hspec $ do
  describe "Verify that individual records are parsed correctly" $ do
    it "parses single record" $ do
      recordsFromText "Agaricus bisporus,cultivated mushroom" `shouldBe` Right [agricusBiporus]

    it "parses single record with additional spaces" $ do
      recordsFromText "  Agaricus bisporus  ,  cultivated mushroom  " `shouldBe` Right [agricusBiporus]

    it "parses single record with multiple common names" $ do
      recordsFromText "Boletus edulis,penny bun / cep" `shouldBe` Right [boletusEdulis]

    it "parses single record with infraspecfic var. x" $ do
      recordsFromText "Boletus luridiformis var. discolor,false yellow bolete" `shouldBe` Right [boletusLuridiformisVarDiscolor]

    it "parses single record with name containing an apostrophe" $ do
      recordsFromText "Bulgaria inquinans,black bulgar / batchelor's buttons" `shouldBe` Right [bulgariaInquinans]

    it "parses single record with sensu stricto" $ do
      recordsFromText "Geopora arenosa s.s,mountain cup" `shouldBe` Right [geoporaArenosa]



agricusBiporus = 
  SourceRecord { scientificName = ScientificName {genus = "Agaricus", species = "bisporus", infraspecific = Nothing, sensu = Nothing}
               , commonNames = ["cultivated mushroom"]
               }

boletusEdulis = 
  SourceRecord { scientificName = ScientificName {genus = "Boletus", species = "edulis", infraspecific = Nothing, sensu = Nothing}
               , commonNames = ["penny bun", "cep"]
               }

boletusLuridiformisVarDiscolor = 
  SourceRecord { scientificName = ScientificName {genus = "Boletus", species = "luridiformis", infraspecific = Just (Variety "discolor"), sensu = Nothing}
               , commonNames = ["false yellow bolete"]
               }

bulgariaInquinans = 
  SourceRecord { scientificName = ScientificName {genus = "Bulgaria", species = "inquinans", infraspecific = Nothing, sensu = Nothing}
               , commonNames = ["black bulgar", "batchelor's buttons"]
               }

geoporaArenosa = 
  SourceRecord { scientificName = ScientificName {genus = "Geopora", species = "arenosa", infraspecific = Nothing, sensu = Just Stricto}
               , commonNames = ["mountain cup"]
               }
