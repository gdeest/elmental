{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Codegen.SampleTypes

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Elmental
import Elmental.Generate

import Test.Hspec
import Test.Hspec.Golden

import Text.Show.Pretty (ppShow)

mkExtractionTest :: forall {k} x. (HasGenerationSpec k x) => String -> Golden String
mkExtractionTest name =
    Golden
        { output = ppShow $ getGenerationSpec @x
        , encodePretty = id
        , writeToFile = writeFile
        , readFromFile = readFile
        , goldenFile = "test-output/extraction/golden/" <> name <> ".txt"
        , actualFile = Just $ "test-output/extraction/actual/" <> name <> ".txt"
        , failFirstTime = True
        }

mkCodegenTest :: [SomeGenerationSpec] -> String -> Golden String
mkCodegenTest typs name =
    Golden
        { output = renderSourceMap $ mkSourceMap typs
        , encodePretty = id
        , writeToFile = writeFile
        , readFromFile = readFile
        , goldenFile = "test-output/codegen/golden/" <> name <> ".txt"
        , actualFile = Just $ "test-output/codegen/actual/" <> name <> ".txt"
        , failFirstTime = True
        }

renderSourceMap :: Map ModuleName Text -> String
renderSourceMap srcMap = unlines (prettifyModule <$> Map.toAscList srcMap)
  where
    prettifyModule (mName, src) =
        unlines
            [ hr
            , Text.unpack mName <> ": "
            , hr
            , Text.unpack src
            , hr
            ]
    hr = replicate 80 '#'

extractionSpec :: Spec
extractionSpec = describe "Extraction" $ do
    it "Handles simple types" $ do
        mkExtractionTest @SimpleType "SimpleType"

    it "Handles simple records" $ do
        mkExtractionTest @SimpleRecord "SimpleRecord"

    it "Handles simple record aliases" $ do
        mkExtractionTest @SimpleRecordAlias "SimpleRecordAlias"

    it "Handles empty aliases" $ do
        mkExtractionTest @EmptyAlias "EmptyAlias"

    it "Handles records with several constructors" $ do
        mkExtractionTest @RecordWithMultipleConstructors "RecordWithMultipleConstructors"

    it "Handles monomorphic recursive types" $ do
        mkExtractionTest @MonomorphicRecursiveType "MonomorphicRecursiveType"

    it "Handles polymorphic recursive types" $ do
        mkExtractionTest @PolymorphicRecursiveType "PolymorphicRecursiveType"

    it "Handles specialized simple HKTs" $ do
        mkExtractionTest @(SimpleHKT Maybe) "SimpleHKT_Maybe"

    it "Handles HKTs with other specialized type variables" $ do
        mkExtractionTest
            @(HKTWithSpecializedKindStarParams Int Text Maybe)
            "HKTWithSpecializedKindStarparams_Int_Text_Maybe"

    it "Handles HKTs with unspecialized type variables" $ do
        mkExtractionTest
            @(HKTWithUnspecializedParams (Either Int))
            "HKTWithUnspecializedParams_(Either_Int)"

    it "Happily ignores phantom parameters of non Type kind" $ do
        mkExtractionTest @(NatPhantomParameter 3) "NatPhantomparameter_3"

    it "Handles large records" $ do
        mkExtractionTest @LargeRecord "LargeRecord"

    it "Handles large sum types" $ do
        mkExtractionTest @CountryCode "CountryCode"

    it "Handles the higher-kinded data pattern (1/2)" $
        mkExtractionTest @(Form 'Submission) "Form_Submission"

    it "Handles the higher-kinded data pattern (2/2)" $
        mkExtractionTest @(Form 'Report) "Form_Report"

generationSpec :: Spec
generationSpec = describe "Generation" $ do
    it "Generates modules for user types" $ do
        mkCodegenTest sampleTypes "SampleTypes"

generate :: IO ()
generate = generateAll "src" sampleTypes

main :: IO ()
main = hspec $ do
    extractionSpec
    generationSpec
