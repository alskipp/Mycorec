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

    it "single record with common name containing '.'" $ do
      parseText "Calocybe gambosa,St. George's Mushroom" `shouldBe` Right [calocybeGambosa]
      
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


  describe "Verify that pretty printing gives correct results for" $ do
    it "sensu stricto" $ do
      ppSensu Stricto `shouldBe` "s.s"

    it "sensu lato" $ do
      ppSensu Lato `shouldBe` "s.l"

    it "infraspecfic SubSpecies" $ do
      ppInfraspecific (SubSpecies "x") `shouldBe` "subsp. x"

    it "infraspecfic Variety" $ do
      ppInfraspecific (Variety "x") `shouldBe` "var. x"

    it "infraspecfic SubVariety" $ do
      ppInfraspecific (SubVariety "x") `shouldBe` "subvar. x"

    it "infraspecfic Forma" $ do
      ppInfraspecific (Forma "x") `shouldBe` "f. x"

    it "infraspecfic SubForma" $ do
      ppInfraspecific (SubForma "x") `shouldBe` "subf. x"

    it "scientificName" $ do
      ppScientificName (scientificName boletusEdulis) `shouldBe` "Boletus edulis"
      
    it "scientificName with infraspecific" $ do
      ppScientificName (scientificName boletusLuridiformisVarDiscolor) `shouldBe` "Boletus luridiformis var. discolor"

    it "scientificName with sensu" $ do
      ppScientificName (scientificName geoporaArenosa) `shouldBe` "Geopora arenosa s.s"


  describe "Verify that sourceRecordToCSV gives correct results for" $ do
    it "simple record" $ do
      sourceRecordToCSV agricusBiporus `shouldBe` "Agaricus bisporus,Cultivated Mushroom"

    it "record with multiple common names" $ do
      sourceRecordToCSV bulgariaInquinans `shouldBe` "Bulgaria inquinans,Black Bulgar / Batchelor's Buttons"

    it "record with infraspecific" $ do
      sourceRecordToCSV boletusLuridiformisVarDiscolor `shouldBe` "Boletus luridiformis var. discolor,False Yellow Bolete"



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

calocybeGambosa = 
  SourceRecord { scientificName = ScientificName {genus = "Calocybe", species = "gambosa", infraspecific = Nothing, sensu = Nothing}
               , commonNames = ["St. George's Mushroom"]
               }
