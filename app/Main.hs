{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import Elmental
import Generics.Kind.TH (deriveGenericK)

import Text.Show.Pretty

-- Beware polykinds
data K (f :: * -> *) (a :: *) = A (f (f (f (f Text)))) | B | C | D

data SomeSimpleType = SomeSimpleType
data SomeHKT a f = SomeHKT
    { field1 :: f a
    , field2 :: Either a Text
    , field3 :: Text
    , field4 :: Text
    , field5 :: Text
    , field6 :: Text
    , field7 :: Text
    }
data SomeType (f :: * -> *) = SomeType (f [Maybe Int])

$(deriveGenericK ''SomeSimpleType)
$(deriveGenericK ''K)
$(deriveGenericK ''SomeHKT)
$(deriveGenericK ''SomeType)
$(deriveGenericK ''[])

-- Derivation for existing datatypes.
$(deriveGenericK ''Maybe)
$(deriveGenericK ''Either)

instance ElmDeclarable (*) SomeSimpleType
instance ElmDeclarable (* -> *) (K Maybe)
instance ElmDeclarable (*) (SomeType (Either Int))
instance ElmDeclarable (* -> *) Maybe
instance ElmDeclarable (*) (SomeHKT Int Maybe) where
    type GetTypeName (*) (SomeHKT Int Maybe) = "SomeHKTMaybe"
    type GetModuleName (*) (SomeHKT Int Maybe) = Just "SomePlace.OtherModule"

instance ElmDeclarable (* -> * -> *) Either where
    type GetTypeName (* -> * -> *) Either = "Either"
    type GetModuleName (* -> * -> *) Either = 'Just "Data.Either"

instance ElmDeclarable (* -> *) [] where
    type GetTypeName (* -> *) [] = "List"
    type GetModuleName (* -> *) [] = 'Nothing

instance ElmDeclarable (*) Text where
    type GetTypeName (*) Text = "String"
    type GetModuleName (*) Text = 'Nothing

instance ElmDeclarable (*) Int where
    type GetTypeName (*) Int = "Int"
    type GetModuleName (*) Int = 'Nothing

instance ElmDeclarable (*) Float where
    type GetTypeName (*) Float = "Float"
    type GetModuleName (*) Float = 'Nothing

instance ElmDeclarable (*) Double where
    type GetTypeName (*) Double = "Float"
    type GetModuleName (*) Double = 'Nothing

main :: IO ()
main = do
    putStrLn $ ppShow $ getElmStructure @(SomeHKT Int Maybe)
    {-
      DatatypeStructure
      { typeName = "SomeHKTMaybe"
      , moduleName = Just "SomePlace.OtherModule"
      , nParams = 0
      , constructors =
          [ Constructor
              { constructorName = "SomeHKT"
              , constructorFields =
                  [ ( Just "field1"
                    , TyRef
                        { tyCon = KnownType ( Just "GHC.Maybe" , "Maybe" )
                        , tyArgs =
                            [ TyRef { tyCon = KnownType ( Nothing , "Int" ) , tyArgs = [] } ]
                        }
                    )
                  , ( Just "field2"
                    , TyRef
                        { tyCon = KnownType ( Just "Data.Either" , "Either" )
                        , tyArgs =
                            [ TyRef { tyCon = KnownType ( Nothing , "Int" ) , tyArgs = [] }
                            , TyRef { tyCon = KnownType ( Nothing , "String" ) , tyArgs = [] }
                            ]
                        }
                    )
                  ]
              }
          ]
      }
    -}

    putStrLn $ ppShow $ getElmStructure @Maybe
    {-
      DatatypeStructure
        { typeName = "Maybe"
        , moduleName = Just "GHC.Maybe"
        , nParams = 1
        , constructors =
            [ Constructor
                { constructorName = "Nothing" , constructorFields = [] }
            , Constructor
                { constructorName = "Just"
                , constructorFields =
                    [ ( Nothing , TyRef { tyCon = TyVar "a0" , tyArgs = [] } ) ]
                }
            ]
        }
    -}

    putStrLn $ ppShow $ getElmStructure @(SomeType (Either Int))
    {-
      DatatypeStructure
      { typeName = "SomeType"
      , moduleName = Just "Main"
      , nParams = 0
      , constructors =
          [ Constructor
              { constructorName = "SomeType"
              , constructorFields =
                  [ ( Nothing
                    , TyRef
                        { tyCon = KnownType ( Just "Data.Either" , "Either" )
                        , tyArgs =
                            [ TyRef { tyCon = KnownType ( Nothing , "Int" ) , tyArgs = [] }
                            , TyRef
                                { tyCon = KnownType ( Nothing , "List" )
                                , tyArgs =
                                    [ TyRef
                                        { tyCon = KnownType ( Just "GHC.Maybe" , "Maybe" )
                                        , tyArgs =
                                            [ TyRef
                                                { tyCon = KnownType ( Nothing , "Int" ) , tyArgs = [] }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    )
                  ]
              }
          ]
      }
    -}

    putStrLn $ ppShow $ getElmStructure @(K Maybe)

-- DatatypeStructure
-- { typeName = "K"
-- , moduleName = Just "Main"
-- , nParams = 1
-- , constructors =
--     [ Constructor
--         { constructorName = "A"
--         , constructorFields =
--             [ ( Nothing
--               , TyRef
--                   { tyCon = KnownType ( Just "GHC.Maybe" , "Maybe" )
--                   , tyArgs =
--                       [ TyRef
--                           { tyCon = KnownType ( Just "GHC.Maybe" , "Maybe" )
--                           , tyArgs =
--                               [ TyRef { tyCon = KnownType ( Nothing , "String" ) , tyArgs = [] }
--                               ]
--                           }
--                       ]
--                   }
--               )
--             ]
--         }
--     , Constructor { constructorName = "B" , constructorFields = [] }
--     , Constructor { constructorName = "C" , constructorFields = [] }
--     , Constructor { constructorName = "D" , constructorFields = [] }
--     ]
-- }
