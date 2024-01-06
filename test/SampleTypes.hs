{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SampleTypes where


import Data.Text (Text)
import Generics.Kind.TH (deriveGenericK)
import GHC.TypeLits (Nat)

data SimpleType = SimpleType Int

data SimpleRecord = SimpleRecord
  { name :: Text
  , age :: SimpleType
  }

data RecordWithMultipleConstructors
  = FirstRecordConstructor { foo :: Bool }
  | SecondRecordConstructor { bar :: Maybe Int }

data MonomorphicRecursiveType = SMRTNil | SMRTCons Int MonomorphicRecursiveType

data PolymorphicRecursiveType (a :: *) = SPRTNil | SPRTCons a (PolymorphicRecursiveType a)

data SimpleHKT (f :: * -> *) = SimpleHKT (f Int)

data HKTWithSpecializedKindStarParams a b f = HKTWithSpecializedKindStarParams b (f a)

data HKTWithUnspecializedParams (f :: * -> *) (a :: *) (b :: *) =
    HKTWithUnspecializedParams (f (f (f a))) b

data NatPhantomParameter (n :: Nat) a = NatPhantomParameter [a]

data LargeRecord a = LargeRecord
    { field0 :: NatPhantomParameter 0 a
    , field1 :: NatPhantomParameter 1 a
    , field2 :: NatPhantomParameter 2 a
    , field3 :: NatPhantomParameter 3 a
    , field4 :: NatPhantomParameter 4 a
    , field5 :: NatPhantomParameter 5 a
    , field6 :: NatPhantomParameter 6 a
    , field7 :: NatPhantomParameter 7 a
    , field8 :: NatPhantomParameter 8 a
    , field9 :: NatPhantomParameter 9 a
    , field10 :: NatPhantomParameter 10 a
    , field11 :: NatPhantomParameter 11 a
    , field12 :: NatPhantomParameter 12 a
    , field13 :: NatPhantomParameter 13 a
    , field14 :: NatPhantomParameter 14 a
    , field15 :: NatPhantomParameter 15 a
    , field16 :: NatPhantomParameter 16 a
    , field17 :: NatPhantomParameter 17 a
    , field18 :: NatPhantomParameter 18 a
    , field19 :: NatPhantomParameter 19 a
    , field20 :: NatPhantomParameter 20 a
    , field21 :: NatPhantomParameter 21 a
    , field22 :: NatPhantomParameter 22 a
    , field23 :: NatPhantomParameter 23 a
    , field24 :: NatPhantomParameter 24 a
    , field25 :: NatPhantomParameter 25 a
    , field26 :: NatPhantomParameter 26 a
    , field27 :: NatPhantomParameter 27 a
    , field28 :: NatPhantomParameter 28 a
    , field29 :: NatPhantomParameter 29 a
    , field30 :: NatPhantomParameter 30 a
    , field31 :: NatPhantomParameter 31 a
    , field32 :: NatPhantomParameter 32 a
    , field33 :: NatPhantomParameter 33 a
    , field34 :: NatPhantomParameter 34 a
    , field35 :: NatPhantomParameter 35 a
    , field36 :: NatPhantomParameter 36 a
    , field37 :: NatPhantomParameter 37 a
    , field38 :: NatPhantomParameter 38 a
    , field39 :: NatPhantomParameter 39 a
    , field40 :: NatPhantomParameter 40 a
    , field41 :: NatPhantomParameter 41 a
    , field42 :: NatPhantomParameter 42 a
    , field43 :: NatPhantomParameter 43 a
    , field44 :: NatPhantomParameter 44 a
    , field45 :: NatPhantomParameter 45 a
    , field46 :: NatPhantomParameter 46 a
    , field47 :: NatPhantomParameter 47 a
    , field48 :: NatPhantomParameter 48 a
    , field49 :: NatPhantomParameter 49 a
    , field50 :: NatPhantomParameter 50 a
    , field51 :: NatPhantomParameter 51 a
    , field52 :: NatPhantomParameter 52 a
    , field53 :: NatPhantomParameter 53 a
    , field54 :: NatPhantomParameter 54 a
    , field55 :: NatPhantomParameter 55 a
    , field56 :: NatPhantomParameter 56 a
    , field57 :: NatPhantomParameter 57 a
    , field58 :: NatPhantomParameter 58 a
    , field59 :: NatPhantomParameter 59 a
    , field60 :: NatPhantomParameter 60 a
    , field61 :: NatPhantomParameter 61 a
    , field62 :: NatPhantomParameter 62 a
    , field63 :: NatPhantomParameter 63 a
    , field64 :: NatPhantomParameter 64 a
    , field65 :: NatPhantomParameter 65 a
    , field66 :: NatPhantomParameter 66 a
    , field67 :: NatPhantomParameter 67 a
    , field68 :: NatPhantomParameter 68 a
    , field69 :: NatPhantomParameter 69 a
    , field70 :: NatPhantomParameter 70 a
    , field71 :: NatPhantomParameter 71 a
    , field72 :: NatPhantomParameter 72 a
    , field73 :: NatPhantomParameter 73 a
    , field74 :: NatPhantomParameter 74 a
    , field75 :: NatPhantomParameter 75 a
    , field76 :: NatPhantomParameter 76 a
    , field77 :: NatPhantomParameter 77 a
    , field78 :: NatPhantomParameter 78 a
    , field79 :: NatPhantomParameter 79 a
    , field80 :: NatPhantomParameter 80 a
    , field81 :: NatPhantomParameter 81 a
    , field82 :: NatPhantomParameter 82 a
    , field83 :: NatPhantomParameter 83 a
    , field84 :: NatPhantomParameter 84 a
    , field85 :: NatPhantomParameter 85 a
    , field86 :: NatPhantomParameter 86 a
    , field87 :: NatPhantomParameter 87 a
    , field88 :: NatPhantomParameter 88 a
    , field89 :: NatPhantomParameter 89 a
    , field90 :: NatPhantomParameter 90 a
    , field91 :: NatPhantomParameter 91 a
    , field92 :: NatPhantomParameter 92 a
    , field93 :: NatPhantomParameter 93 a
    , field94 :: NatPhantomParameter 94 a
    , field95 :: NatPhantomParameter 95 a
    , field96 :: NatPhantomParameter 96 a
    , field97 :: NatPhantomParameter 97 a
    , field98 :: NatPhantomParameter 98 a
    , field99 :: NatPhantomParameter 99 a
    }

-- data SimpleType = SimpleType Int

-- data SimpleRecord = SimpleRecord
--   { name :: Text
--   , age :: SimpleType
--   }

-- data RecordWithMultipleConstructors
--   = FirstRecordConstructor { foo :: Bool }
--   | SecondRecordConstructor { bar :: Maybe Int }

-- data MonomorphicRecursiveType = SMRTNil | SMRTCons Int MonomorphicRecursiveType

-- data PolymorphicRecursiveType (a :: *) = SPRTNil | SPRTCons a (PolymorphicRecursiveType a)

-- data SimpleHKT (f :: * -> *) = SimpleHKT (f Int)

-- data HKTWithSpecializedKindStarParams a f = HKTWithSpecializedKindStarParams (f a)

-- data HKTWithUnspecializedParam (f :: * -> *) (a :: *) =
--     HKTWithUnspecializedParam (f (f (f a)))
$(deriveGenericK ''SimpleType)
$(deriveGenericK ''SimpleRecord)
$(deriveGenericK ''RecordWithMultipleConstructors)
$(deriveGenericK ''MonomorphicRecursiveType)
$(deriveGenericK ''PolymorphicRecursiveType)
$(deriveGenericK ''SimpleHKT)
$(deriveGenericK ''HKTWithSpecializedKindStarParams)
$(deriveGenericK ''HKTWithUnspecializedParams)
$(deriveGenericK ''NatPhantomParameter)
$(deriveGenericK ''LargeRecord)

-- Datatypes defined elsewhere
$(deriveGenericK ''Bool)
$(deriveGenericK ''Maybe)
$(deriveGenericK ''Either)
