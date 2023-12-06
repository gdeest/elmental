{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elmental
  ( ElmDeclarable (..)
  , HasElmStructure
  , ElmMapping (..)
  , HasSymbolInfo
  , ElmKind
  , defaultMapping
  , getElmStructure
  , getTypeName
  , getModuleName
  , getMapping
  , setModule
  , module Elmental.ElmStructure
  ) where

import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Elmental.ElmStructure
import GHC.Generics qualified as GHC
import GHC.TypeLits
import Generics.Kind


{- | Class mapping a Haskell type constructor @x :: k@ to an Elm type constructor.

  You can define instances for this class for any Haskell data / newtype constructor,
  be it unapplied, partially applied or fully applied, provided that its kind is
  not Elm-compatible (i.e., not higher-kinded).

  For example:

  @
    data SomeHKT f a = SomeHKT (f a)

    instance ElmDeclarable ((Type -> Type) -> Type) SomeHKT -- Not OK: SomeHKT is higher-kinded.
    instance ElmDeclarable (Type -> Type) (SomeHKT Maybe) -- OK
    instance ElmDeclarable Type (SomeHKT Maybe Int) -- OK

    instance ElmDeclarable [] -- OK
    instance ElmDeclarable [Char] -- OK
 @
-}
class ElmKind k => ElmDeclarable k (x :: k) where
  -- | Elm mapping information.
  --
  --  Contains the name / location of the type and its encoder / decoder.
  --  Can be overridden.
  --
  --  Example:
  --
  --  @
  --    instance ElmDeclarable Type [Char] where
  --      mapTo = ElmMapping
  --        { typeName = "String"
  --        , moduleName = Nothing
  --        , encoderLocation = Just $ SymbolLocation
  --            { symbolName = "string"
  --            , moduleName = "Json.Encode"
  --            }
  --        , decoderLocation = Just $ SymbolLocation
  --            { symbolName = "string"
  --            , moduleName = "Json.Decode"
  --            }
  --        , args = []
  --        }
  --  @
  mapTo :: ElmMapping
  default mapTo :: (HasSymbolInfo x) => ElmMapping
  mapTo = defaultMapping @x


  -- | Internal function. You should not have to define this method yourself.
  mkTyRef :: PList (NParams k) TyRef -> TyRef
  default mkTyRef :: PList (NParams k) TyRef -> TyRef
  mkTyRef pList = TyRef (TyMapping (mapTo @k @x)) (pListToList pList)


{- | Instance for applied type constructors.

  Necessary to traverse the list of type constructors down to the root when constructing
  type references to applied type constructors.
-}
instance
  {-# OVERLAPPABLE #-}
  forall k (t :: Type) (x :: Type -> k).
  ( ElmDeclarable Type t
  , ElmDeclarable (Type -> k) (x :: Type -> k)
  ) =>
  ElmDeclarable k (x t)
  where
  mapTo =
    let tMapping = mapTo @_ @t
        xMapping = mapTo @_ @x
     in xMapping {args = xMapping.args <> [tMapping]}


  mkTyRef remainingParams = mkTyRef @_ @x ((mkTyRef @_ @t PNil) `PCons` remainingParams)


type HasSymbolInfo x =
  ( KnownSymbol (GetTypeNameG (RepK x))
  , KnownSymbol (GetModuleNameG (RepK x))
  )


defaultMapping :: forall x. HasSymbolInfo x => ElmMapping
defaultMapping =
  ElmMapping
    { typeName = tName
    , moduleName = Just mName
    , encoderLocation =
        Just $
          SymbolLocation
            { symbolName = "encode" <> tName
            , symbolModuleName = mName
            }
    , decoderLocation =
        Just $
          SymbolLocation
            { symbolName = "decode" <> tName
            , symbolModuleName = mName
            }
    , args = []
    , isTypeAlias = False
    , urlPiece = Nothing
    , queryParam = Nothing
    }
 where
  tName = symbolToText @(GetTypeNameG (RepK x))
  mName = symbolToText @(GetModuleNameG (RepK x))


{- | Overrides / sets the module name everywhere in a mapping.
 Often useful in conjunction wit @defaultMapping@.
-}
setModule :: Text -> ElmMapping -> ElmMapping
setModule moduleName mapping =
  mapping
    { moduleName = Just moduleName
    , decoderLocation =
        ( \l ->
            l
              { symbolModuleName = moduleName
              }
        )
          <$> mapping.decoderLocation
    , encoderLocation =
        ( \l ->
            l
              { symbolModuleName = moduleName
              }
        )
          <$> mapping.encoderLocation
    }


-- Type metadata utilities
type family GetModuleNameG x where
  GetModuleNameG (M1 _d ('GHC.MetaData _tyConName moduleName _pkg _isNewtype) _sop) = moduleName


type family GetTypeNameG x where
  GetTypeNameG (M1 _d ('GHC.MetaData tyConName _moduleName _pkg _isNewtype) _sop) = tyConName


symbolToText :: forall sym. KnownSymbol sym => Text
symbolToText = Text.pack $ symbolVal (Proxy @sym)


getMapping :: forall {k} x. ElmDeclarable k x => ElmMapping
getMapping = mapTo @k @x


getTypeName :: forall {k} x. ElmDeclarable k x => Text
getTypeName = (getMapping @x).typeName


getModuleName :: forall {k} x. ElmDeclarable k x => Maybe Text
getModuleName = (getMapping @x).moduleName


-- Usual Peano numbers / length-indexed lists stuff.
data PNat = Z | S PNat


type family PNatToNat (n :: PNat) :: Natural where
  PNatToNat Z = 0
  PNatToNat (S n) = 1 + PNatToNat n


data PList (n :: PNat) a where
  PNil :: PList Z a
  PCons :: a -> PList n a -> PList (S n) a


pListToList :: PList n a -> [a]
pListToList PNil = []
pListToList (a `PCons` as) = a : (pListToList as)


-- | Constraint establishing that a kind is valid in Elm.
type ElmKind k = ElmKindB k ~ True


type family ElmKindB k :: Bool where
  ElmKindB Type = True
  ElmKindB (Type -> k) = ElmKindB k


-- Compute the number of type parameters of a type constuctor.
type family NParams k :: PNat where
  NParams Type = Z
  NParams (Type -> k) = S (NParams k)


-- This shouldn't have to be a class as it only has a single instance,
-- but it seems to be the only way to expose @HasElmStructure@ as a simple
-- constraint.
class (repK ~ RepK x) => HasElmStructure' k (x :: k) repK where
  getElmStructure' :: DatatypeStructure x


instance
  ( ElmDeclarable k x
  , RepK x ~ M1 GHC.D ('GHC.MetaData tName mName pkg isNewtype) sop
  , GElmSum sop
  , KnownNat (PNatToNat (NParams k))
  ) =>
  HasElmStructure' k x (M1 GHC.D ('GHC.MetaData tName mName pkg isNewtype) sop)
  where
  getElmStructure' =
    DatatypeStructure
      { mapping = getMapping @x
      , nParams = natVal $ Proxy @(PNatToNat (NParams k))
      , constructors = getValueConstructors @_ @sop
      }


type HasElmStructure k x = HasElmStructure' k x (RepK x)


{- | Extract the structure of the representation of a datatype in Elm.

Used by code generation.
-}
getElmStructure :: forall {k} (x :: k). (HasElmStructure k x) => DatatypeStructure x
getElmStructure = getElmStructure' @k @x @(RepK x)


-- Extraction logic.
--
-- We essentially pattern-match on the Generic representation to extract:
--
-- - Elm metadata attached via the @ElmDeclarable@ class (type name and module name).
-- - Constructors (GElmSum).
-- - Fields, their names, and their types (potentially involving type variables).

class GElmSum (sop :: k) where
  getValueConstructors :: [Constructor]


instance
  ( KnownSymbol valConName
  , GElmProduct fields
  ) =>
  GElmSum (M1 GHC.C ('GHC.MetaCons valConName 'GHC.PrefixI isNt) fields)
  where
  getValueConstructors =
    [ Constructor
        { constructorName = symbolToText @valConName
        , constructorFields = getFields @_ @fields
        }
    ]


instance
  ( KnownSymbol valConName
  , GElmProduct fields
  , GElmSum otherCons
  ) =>
  GElmSum (M1 GHC.C ('GHC.MetaCons valConName 'GHC.PrefixI isNt) fields :+: otherCons)
  where
  getValueConstructors =
    ( Constructor
        { constructorName = Text.pack $ symbolVal (Proxy @valConName)
        , constructorFields = getFields @_ @fields
        }
    )
      : getValueConstructors @_ @otherCons


instance
  ( GElmSum (s1 :+: s2)
  , GElmSum otherCons
  ) =>
  GElmSum ((s1 :+: s2) :+: otherCons)
  where
  getValueConstructors =
    getValueConstructors @_ @(s1 :+: s2)
      ++ getValueConstructors @_ @otherCons


class GElmProduct (fields :: k) where
  getFields :: [ElmField]


instance GElmProduct U1 where
  getFields = []


instance
  (GElmField (M1 GHC.S ('GHC.MetaSel mbFName u s l) fieldType)) =>
  GElmProduct (M1 GHC.S ('GHC.MetaSel mbFName u s l) fieldType)
  where
  getFields = [getField @(M1 GHC.S ('GHC.MetaSel mbFName u s l) fieldType)]


instance (GElmProduct (f1 :*: f2), GElmProduct fields) => GElmProduct ((f1 :*: f2) :*: fields) where
  getFields = (getFields @_ @(f1 :*: f2)) ++ (getFields @_ @fields)


instance (GElmField (M1 s m t), GElmProduct fields) => GElmProduct ((M1 s m t) :*: fields) where
  getFields = (getField @(M1 s m t)) : (getFields @_ @fields)


class GElmField field where
  getField :: ElmField


instance
  (GElmFieldType Z fieldType, KnownSymbol fieldName) =>
  GElmField (M1 GHC.S ('GHC.MetaSel (Just fieldName) u s l) (Field fieldType))
  where
  getField = (Just (symbolToText @fieldName), getTyRef @Z @fieldType PNil)


instance
  (GElmFieldType Z fieldType) =>
  GElmField (M1 GHC.S ('GHC.MetaSel Nothing u s l) (Field fieldType))
  where
  getField = (Nothing, getTyRef @Z @fieldType PNil)


class GElmFieldType (nParams :: PNat) fieldType where
  getTyRef :: PList nParams TyRef -> TyRef


class HasNat (vn :: k) where
  type ToNat vn :: Nat


-- GHC refuses a simple type family declaration (probably because the kinds vary).
instance HasNat VZ where
  type ToNat VZ = 0
instance HasNat (VS vn) where
  type ToNat (VS vn) = 1 + (ToNat vn)


instance (KnownNat (ToNat vn)) => GElmFieldType Z ('Var vn) where
  getTyRef _ = TyRef (TyVar $ "a" <> Text.pack (show $ natVal $ Proxy @(ToNat vn))) []


instance (ElmDeclarable k someType, nParams ~ NParams k) => GElmFieldType nParams ('Kon someType) where
  getTyRef params = mkTyRef @k @someType params


instance
  (GElmFieldType Z t2, GElmFieldType (S n) t1) =>
  GElmFieldType n (t1 :@: t2)
  where
  getTyRef params = getTyRef @(S n) @t1 ((getTyRef @Z @t2 PNil) `append` params)


append :: a -> PList n a -> PList (S n) a
append a PNil = a `PCons` PNil
append a (b `PCons` bs) = (b `PCons` (a `append` bs))
