{-# LANGUAGE OverloadedStrings #-}

module Main where

import Mycorec
import SourceRecord
import SourceRecordParser
import Test.Hspec
import Data.Either


parseText = parseLines parseSourceRecord

main :: IO ()
main = hspec $ do
  describe "Verify that individual records are successfully parsed containing" $ do
    it "single record" $ do
      parseText "Agaricus bisporus,cultivated mushroom" `shouldBe` Right [agricusBiporus]

    it "single record with additional spaces" $ do
      parseText "  Agaricus bisporus  ,  cultivated mushroom  " `shouldBe` Right [agricusBiporus]

    it "single record with multiple common names" $ do
      parseText "Boletus edulis,penny bun / cep" `shouldBe` Right [boletusEdulis]

    it "single record with one word common name" $ do
      parseText "Thelephora terrestris,Earthfan" `shouldBe` Right [thelephoraTerrestris]

    it "single record with infraspecfic var. x" $ do
      parseText "Boletus luridiformis var. discolor,false yellow bolete" `shouldBe` Right [boletusLuridiformisVarDiscolor]

    it "single record with sensu stricto" $ do
      parseText "Geopora arenosa s.s,mountain cup" `shouldBe` Right [geoporaArenosa]

    it "single record with name containing an apostrophe" $ do
      parseText "Bulgaria inquinans,black bulgar / batchelor's buttons" `shouldBe` Right [bulgariaInquinans]

    it "single record with name containing a hyphen" $ do
      parseText "Puccinia eutremae,Scurvy-grass Rust" `shouldBe` Right [pucciniaEutremae]


  describe "Verify that malformed records fail to parse containing" $ do
    it "disallowed characters" $ do
      parseText "Aga#ricus bis+porus,cultivated mushroom" `shouldSatisfy` isLeft


  describe "Verify that multiple lines of records are parsed correctly with" $ do
    it "single line ends" $ do
      fmap length (parseText "Abc Def,xyz\nAaa Bbb,Zzz") `shouldBe` Right 2

    it "parses records with extra line ends" $ do
      fmap length (parseText "Abc Def,xyz\n\n\nAaa Bbb,Zzz") `shouldBe` Right 2



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

pucciniaEutremae = 
  SourceRecord { scientificName = ScientificName {genus = "Puccinia", species = "eutremae", infraspecific = Nothing, sensu = Nothing}
               , commonNames = ["Scurvy-grass Rust"]
               }

thelephoraTerrestris = 
  SourceRecord { scientificName = ScientificName {genus = "Thelephora", species = "terrestris", infraspecific = Nothing, sensu = Nothing}
               , commonNames = ["Earthfan"]
               }
