{-# LANGUAGE NoFieldSelectors #-}

module Elmental.GenerationSpec where

import Data.Text (Text)

data GenerationSpec a = GenerationSpec
    { mapping :: ElmMapping
    , nParams :: Integer
    , constructors :: [Constructor]
    }
    deriving (Eq, Show, Ord)

{- | Contains the mapping of Haskell type constructor to an Elm type constructor,
and potentially the location of its encoder / decoder.
-}
data ElmMapping = ElmMapping
    { typeName :: TypeName
    -- ^ Name of the corresponding Elm datatype.
    , moduleName :: Maybe ModuleName
    -- ^ Name of the module this type should be imported from / generated in.
    --
    --  Can be set to @Nothing@ for core Elm types (@List@, @Bool@, @String@, ...)
    --  which don't require explicit imports.
    , encoderLocation :: Maybe SymbolLocation
    -- ^ Location of the encoder for this type.
    --
    --  Must be set if you want to generate it, or to generate encoders depending on it.
    , decoderLocation :: Maybe SymbolLocation
    -- ^ Location of the decoder for this type.
    --
    --  Must be set if you want to generate it, or to generate encoders depending on it.
    , args :: [ElmMapping]
    -- ^ Arguments supplied to the type constructor.
    , isTypeAlias :: Bool
    -- ^ Indicates whether the generated Elm datataype should be a type alias
    }
    deriving (Eq, Show, Ord)

type ModuleName = Text
type SymbolName = Text
type TypeName = Text
type ConstructorName = Text
type FieldName = Text
type VarName = Text

-- | Location of an Elm value (symbol / module name).
data SymbolLocation = SymbolLocation
    { symbolName :: SymbolName
    , symbolModuleName :: ModuleName
    }
    deriving (Eq, Show, Ord)

data TyCon
    = TyMapping ElmMapping
    | TyVar VarName
    deriving (Eq, Show, Ord)

data TyRef = TyRef
    { tyCon :: TyCon
    , tyArgs :: [TyRef]
    }
    deriving (Eq, Show, Ord)

type ElmField =
    (Maybe FieldName, TyRef)

data Constructor = Constructor
    { constructorName :: ConstructorName
    , constructorFields :: [ElmField]
    }
    deriving (Eq, Show, Ord)
