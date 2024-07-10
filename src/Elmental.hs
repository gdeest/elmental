
{- | == Introduction

[Data transfer objects](https://en.wikipedia.org/wiki/Data_transfer_object) are
crucial components of a web API interface. Inconsistencies between the format
expected by a web client and that assumed by the server often lead to runtime
errors. Elmental simplifies the task of maintaining synchronization between
backend and frontend definitions. It achieves this by automatically generating
frontend types along with associated JSON encoders/decoders directly from
backend definitions.

Key features that set Elmental apart include:

- Seamless integration with existing Elm types, including legacy and third-party
  types. Specific handling for Haskell types is straightforward; simply specify
  the location of the corresponding Elm type and encoder/decoder within your
  frontend codebase or dependencies.

- Native support for polymorphic data types and powerful Haskell idioms, leveraging
  the capabilities of
  [kind-generics](https://hackage.haskell.org/package/kind-generics).

=== Basic Usage

You need
[kind-generics](https://hackage.haskell.org/package/kind-generics) and
[kind-generics-th](https://hackage.haskell.org/package/kind-generics-th) added
as dependencies in your `.cabal` file.

==== 1) Define your DTO and derive 'Generics.Kind.GenericK'

@
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE KindSignatures #-}
  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE TypeFamilies #-}

  module MyApp.API.DTOs where

  import GHC.Generics (Generic)
  import Generics.Kind
  import Generics.Kind.TH

  -- Define a polymorphic Data Transfer Object
  data MyDTO a = MyDTO a Int

  -- Use Template Haskell to derive GenericK
  deriveGenericK ''MyDTO
@

==== 2) Define its Elm Mapping

@
  import Elmental

  instance ElmDeclarable MyDTO
    mapTo = defaultMapping @MyDTO
      { moduleName = Just "CustomModuleName" }
@

==== 3) Generate the code !

@
  {-# LANGUAGE TypeApplications #-}

  module Main (main) where

  import Elmental.Generate
  import MyApp.API.DTOs (MyDTO)

  main :: IO ()
  main = generateAll "src" [ include @MyDTO ]
@

-}
module Elmental (
    ElmDeclarable (..),
    HasGenerationSpec,
    ElmMapping (..),
    SomeGenerationSpec,
    HasSymbolInfo,
    ElmKind,
    defaultMapping,
    setModule,
    include,
    generateAll,
    getGenerationSpec,
    getTypeName,
    module Elmental.GenerationSpec,
) where

import Elmental.Extract
import Elmental.GenerationSpec
import Elmental.Generate
-- import Data.Kind (Type)
-- import Data.Proxy
-- import Data.Text (Text)
-- import Data.Text qualified as Text
-- import Elmental.ElmStructure
-- import GHC.Generics qualified as GHC
-- import GHC.TypeLits
-- import Generics.Kind
