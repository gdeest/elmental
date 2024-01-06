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

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Elmental
import Elmental.Generate

import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.Process qualified as Process
import Test.Hspec
import Test.Hspec.Golden

import Text.Show.Pretty (ppShow)


instance ElmDeclarable Type SimpleType
instance ElmDeclarable Type SimpleRecord
instance ElmDeclarable Type RecordWithMultipleConstructors
instance ElmDeclarable Type MonomorphicRecursiveType
instance ElmDeclarable (Type -> Type) PolymorphicRecursiveType
instance (ElmDeclarable (Type -> Type) f, HasSymbolInfo f) => ElmDeclarable Type (SimpleHKT f) where
  mapTo =
    (defaultMapping @(SimpleHKT f))
      { typeName = "SimpleHKT" <> getTypeName @f
      }


instance ElmDeclarable Type (HKTWithSpecializedKindStarParams Int Text Maybe)
instance ElmDeclarable (Type -> Type -> Type) (HKTWithUnspecializedParams (Either Int))


instance ElmDeclarable (Type -> Type) (NatPhantomParameter n) where
  mapTo =
    (defaultMapping @(NatPhantomParameter n))
      { typeName = "LookMaNoPhantomParam"
      }


instance ElmDeclarable (Type -> Type) LargeRecord


instance ElmDeclarable Type Int where
  mapTo =
    ElmMapping
      { typeName = "Int"
      , moduleName = Nothing
      , decoderLocation =
          Just $
            SymbolLocation
              { symbolName = "int"
              , symbolModuleName = "Json.Decode"
              }
      , encoderLocation =
          Just $
            SymbolLocation
              { symbolName = "int"
              , symbolModuleName = "Json.Encode"
              }
      , args = []
      , isTypeAlias = False
      , urlPiece = Nothing
      , queryParam = Nothing
      }


instance ElmDeclarable Type Text where
  mapTo =
    ElmMapping
      { typeName = "String"
      , moduleName = Nothing
      , decoderLocation =
          Just $
            SymbolLocation
              { symbolName = "string"
              , symbolModuleName = "Json.Decode"
              }
      , encoderLocation =
          Just $
            SymbolLocation
              { symbolName = "string"
              , symbolModuleName = "Json.Encode"
              }
      , args = []
      , isTypeAlias = False
      , urlPiece = Nothing
      , queryParam = Nothing
      }


instance ElmDeclarable Type [Char] where
  mapTo = mapTo @_ @Text


instance ElmDeclarable Type Bool where
  mapTo =
    ElmMapping
      { typeName = "Bool"
      , moduleName = Nothing
      , decoderLocation =
          Just $
            SymbolLocation
              { symbolName = "bool"
              , symbolModuleName = "Json.Decode"
              }
      , encoderLocation =
          Just $
            SymbolLocation
              { symbolName = "bool"
              , symbolModuleName = "Json.Encode"
              }
      , args = []
      , isTypeAlias = False
      , urlPiece = Nothing
      , queryParam = Nothing
      }


instance ElmDeclarable (Type -> Type) Maybe where
  mapTo =
    ElmMapping
      { typeName = "Maybe"
      , moduleName = Just "Maybe"
      , decoderLocation =
          Just $
            SymbolLocation
              { symbolName = "nullable"
              , symbolModuleName = "Json.Decode"
              }
      , encoderLocation =
          Just $
            SymbolLocation
              { symbolName = "maybe"
              , symbolModuleName = "Json.Encode.Extra"
              }
      , args = []
      , isTypeAlias = False
      , urlPiece = Nothing
      , queryParam = Nothing
      }


instance ElmDeclarable (Type -> Type -> Type) Either where
  mapTo = setModule "Codegen.Either" (defaultMapping @Either)


instance ElmDeclarable (Type -> Type) [] where
  mapTo =
    ElmMapping
      { typeName = "List"
      , moduleName = Nothing
      , decoderLocation =
          Just $
            SymbolLocation
              { symbolName = "list"
              , symbolModuleName = "Json.Decode"
              }
      , encoderLocation =
          Just $
            SymbolLocation
              { symbolName = "list"
              , symbolModuleName = "Json.Encode"
              }
      , args = []
      , isTypeAlias = False
      , urlPiece = Nothing
      , queryParam = Nothing
      }


instance ElmDeclarable Type (Form 'Submission) where
  mapTo = setModule "Codegen.Submission" (defaultMapping @(Form 'Submission))


instance ElmDeclarable Type (Form 'Report) where
  mapTo = setModule "Codegen.Report" (defaultMapping @(Form 'Report))


instance ElmDeclarable Type (FileUpload 'Submission) where
  mapTo = setModule "Codegen.Submission" (defaultMapping @(FileUpload 'Submission))


instance ElmDeclarable Type (FileUpload 'Report) where
  mapTo = setModule "Codegen.Report" (defaultMapping @(FileUpload 'Report))


instance ElmDeclarable Type SimpleRecordAlias where
  mapTo =
    (defaultMapping @SimpleRecordAlias)
      { isTypeAlias = True
      }


instance ElmDeclarable Type EmptyAlias where
  mapTo =
    (defaultMapping @EmptyAlias)
      { isTypeAlias = True
      }


mkExtractionTest :: forall {k} x. HasElmStructure k x => String -> Golden String
mkExtractionTest name =
  Golden
    { output = ppShow $ getElmStructure @x
    , encodePretty = id
    , writeToFile = writeFile
    , readFromFile = readFile
    , goldenFile = "test-output/extraction/golden/" <> name <> ".txt"
    , actualFile = Just $ "test-output/extraction/actual/" <> name <> ".txt"
    , failFirstTime = True
    }


mkCodegenTest :: [SomeStructure] -> String -> Golden String
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
extractionSpec = do
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

  -- it "Handles large sum types" $ do
  --   mkExtractionTest @CountryCode "CountryCode"

  it "Handles the higher-kinded data pattern (1/2)" $
    mkExtractionTest @(Form 'Submission) "Form_Submission"

  it "Handles the higher-kinded data pattern (2/2)" $
    mkExtractionTest @(Form 'Report) "Form_Report"


-- sampleTypes :: [SomeStructure]
-- sampleTypes =
--   [ include @SimpleType
--   , include @SimpleRecord
--   , include @SimpleRecordAlias
--   , include @EmptyAlias
--   , include @MonomorphicRecursiveType
--   , include @PolymorphicRecursiveType
--   , include @(SimpleHKT Maybe)
--   , include @(HKTWithSpecializedKindStarParams Int Text Maybe)
--   , include @(HKTWithUnspecializedParams (Either Int))
--   , include @(NatPhantomParameter 3)
--   , include @RecordWithMultipleConstructors
--   , include @LargeRecord
--   , include @Either
--   , include @(FileUpload 'Submission)
--   , include @(FileUpload 'Report)
--   , include @(Form 'Submission)
--   , include @(Form 'Report)
--   ]


generationSpec :: Spec
generationSpec = do
  it "Generates modules for user types" $ do
    mkCodegenTest sampleTypes "SampleTypes"

generate :: IO ()
generate = generateAll "src" sampleTypes

main :: IO ()
main = do
  hspec extractionSpec
  hspec generationSpec
  -- generateAll "test-app/src" sampleTypes
  -- withCurrentDirectory "test-app/" $ do
  --   ExitSuccess <- Process.rawSystem "elm" ["make", "src/Main.elm"]
  --   pure ()
