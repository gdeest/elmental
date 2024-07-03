{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Elmental.Generate (
    generateTypeDef,
    generateEncoder,
    generateDecoder,
    generateAll,
    computeAll,
    mkSourceMap,
    include,
    outputModule,
    SomeGenerationSpec (..),
    ModuleDefinition (..),
) where

import Elmental.GenerationSpec
import Elmental.Extract

import Data.Foldable (toList, traverse_)
import Data.Function ((&))
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import NeatInterpolation (trimming, untrimming)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Generate a type definition
generateTypeDef :: forall {k} (x :: k). (HasGenerationSpec k x) => Text
generateTypeDef = generateTypeDef' $ getGenerationSpec @x

generateTypeDef' :: GenerationSpec x -> Text
generateTypeDef' GenerationSpec{..}
    | (mapping.isTypeAlias && length constructors == 1 && null mapping.args && nParams == 0) =
        [trimming|
    type alias $tName =
      $aliasDef
  |]
    | (mapping.isTypeAlias && (length constructors /= 1 || not (null mapping.args) || nParams /= 0)) =
        error $ "Cannot generate an Elm type alias for: " <> show mapping.typeName
    | (not (null constructors) && null mapping.args) =
        [trimming|
    type $tName $tVars
      = $constructorDefs
  |]
  where
    tName = mapping.typeName
    currentModule = case mapping.moduleName of
        Nothing -> error $ "Cannot generate mapping for: " <> show mapping.typeName <> " (unknown module)"
        Just mName -> mName
    tVars = Text.intercalate " " (nToVarName <$> take (fromIntegral nParams) [0 ..])
    aliasDef = Text.intercalate "\n| " (renderAlias <$> constructors)
    constructorDefs = Text.intercalate "\n| " (renderConstructor <$> constructors)
    renderAlias Constructor{..} =
        case constructorFields of
            [] -> [trimming|{}|]
            _ ->
                let
                    renderedRecordFields = renderRecordFields constructorFields
                 in
                    [trimming|$renderedRecordFields|]

    renderConstructor Constructor{..} =
        case constructorFields of
            [] -> constructorName
            ((Nothing, _) : _) ->
                [trimming|
            $constructorName $renderedAnonymousFields
          |]
            ((Just _, _) : _) ->
                [trimming|
            $constructorName
                $renderedRecordFields
          |]
      where
        renderedAnonymousFields = renderAnonymousFields constructorFields
        renderedRecordFields = renderRecordFields constructorFields

    renderAnonymousFields = Text.intercalate " " . map (renderTyRef currentModule . snd)
    renderRecordFields fields =
        [trimming|
        { $renderedRecordFields
        }
      |]
      where
        renderedRecordFields = Text.intercalate "\n, " (renderField <$> fields)
        renderField (Just fieldName, tyRef) = mconcat [fieldName, " : ", renderTyRef currentModule tyRef]
        renderField a = error ("renderField - unmatched pattern:" ++ show a)

    nToVarName = Text.pack . ('a' :) . show @Integer
generateTypeDef' _ = error "Datatype has no constructor. Impossible to generate Elm definition."

-- | Generate a decoder
generateDecoder :: forall {k} (x :: k). (HasGenerationSpec k x) => Text
generateDecoder = generateDecoder' $ getGenerationSpec @x

generateDecoder' :: GenerationSpec x -> Text
generateDecoder' GenerationSpec{..} =
    [trimming|
    $decoderName : $decoderType
    $decoderName $decoderArgs =
      $decoderBody
  |]
  where
    decoderName = case mapping.decoderLocation of
        Just location -> location.symbolName
        Nothing -> error $ "No decoder location for: " <> show mapping.typeName
    decoderType =
        mconcat
            [ mconcat $ (\n -> "Decoder a" <> n <> " -> ") <$> params
            , "Decoder " <> fullTypeName
            ]
    decoderArgs =
        Text.intercalate " " $ ("d" <>) <$> params
    decoderBody = case constructors of
        [] ->
            error $ "Uninhabited datatype: " <> show mapping.typeName
        [singleConstructor] ->
            if mapping.isTypeAlias
                then decodeRecordAlias (mapping.typeName, mapping.moduleName) (singleConstructor.constructorFields)
                else decodeConstructor (mapping.typeName, mapping.moduleName) singleConstructor
        multiple ->
            (if all isNullary constructors then decodeStringTag fullTypeName ((.constructorName) <$> constructors) else decodeTaggedSum (mapping.typeName, mapping.moduleName) fullTypeName multiple)
    fullTypeName =
        case nParams of
            0 -> mapping.typeName
            _ ->
                let tVars = Text.intercalate " " $ ("a" <>) <$> params
                 in mconcat ["(", mapping.typeName, " ", tVars, ")"]

    params = (Text.pack . show @Integer) <$> take (fromIntegral nParams) [0 ..]

isNullary :: Constructor -> Bool
isNullary Constructor{..} = null constructorFields

decodeStringTag :: Text -> [ConstructorName] -> Text
decodeStringTag fullTypeName cNames =
    let tagBranches =
            Text.intercalate "\n" $
                (\name -> mconcat ["\"", name, "\" -> Json.Decode.succeed ", name])
                    <$> cNames
     in [trimming|
    let
      decide : String -> Decoder $fullTypeName
      decide tag =
        case tag of
          $tagBranches
          other ->
            Json.Decode.fail <| "$fullTypeName doesn't have constructor: " ++ other
    in
    Json.Decode.string
      |> Json.Decode.andThen decide
  |]

decodeTaggedSum :: (TypeName, Maybe ModuleName) -> Text -> [Constructor] -> Text
decodeTaggedSum recursionStop fullTypeName constructors =
    let tagBranches = Text.intercalate "\n" $ formatTagBranch recursionStop <$> constructors
     in [trimming|
    let
      decide : String -> Decoder $fullTypeName
      decide tag =
        case tag of
          $tagBranches
          other ->
            Json.Decode.fail <| "$fullTypeName doesn't have constructor: " ++ other
    in
    Json.Decode.field "tag" Json.Decode.string
     |> Json.Decode.andThen decide
  |]

formatTagBranch :: (TypeName, Maybe ModuleName) -> Constructor -> Text
formatTagBranch recursionStop constructor@Constructor{..} =
    let objectDecoder = decodeConstructor recursionStop constructor
     in [trimming|
    "$constructorName" ->
      $objectDecoder
  |]

decodeConstructor :: (TypeName, Maybe ModuleName) -> Constructor -> Text
decodeConstructor recursionStop Constructor{..} =
    case constructorFields of
        [] -> "Json.Decode.succeed " <> constructorName
        [(Nothing, tyRef)] ->
            mconcat
                [ [trimming|Json.Decode.field "contents" <||]
                , " Json.Decode.map "
                , constructorName
                , " "
                , mkTypeDecoder recursionStop tyRef
                ]
        fields@((Nothing, _) : _) -> decodeAnonymousConstructor recursionStop constructorName fields
        fields@((Just _, _) : _) -> decodeRecordConstructor recursionStop constructorName fields

decodeRecordConstructor :: (TypeName, Maybe ModuleName) -> ConstructorName -> [ElmField] -> Text
decodeRecordConstructor recursionStop cName fields =
    case fields of
        [] -> error "decodeRecordConstructor called on nullary constructor"
        fields' ->
            let mkFunction = mkFieldsFunction fields'
                fieldDecoders =
                    Text.intercalate "\n" $
                        mkFieldDecoder recursionStop <$> fields
             in [trimming|
        let mkFunction =
              $mkFunction
            contentDecoder =
              Json.Decode.succeed mkFunction
                $fieldDecoders
        in Json.Decode.field "contents" (Json.Decode.map $cName contentDecoder)
      |]

decodeRecordAlias :: (TypeName, Maybe ModuleName) -> [ElmField] -> Text
decodeRecordAlias recursionStop fields =
    case fields of
        [] -> [trimming| Json.Decode.succeed {} |]
        fields' ->
            let mkFunction = mkFieldsFunction fields'
                fieldDecoders =
                    Text.intercalate "\n" $
                        mkFieldDecoder recursionStop <$> fields
             in [trimming|
        let mkFunction =
              $mkFunction
            contentDecoder =
              Json.Decode.succeed mkFunction
                $fieldDecoders
        in contentDecoder
      |]

mkFieldDecoder :: (TypeName, Maybe ModuleName) -> ElmField -> Text
mkFieldDecoder recursionStop (Just fieldName, tyRef) =
    let typDecoder = mkTypeDecoder recursionStop tyRef
     in [trimming|
    |> andMap (Json.Decode.field "$fieldName" $typDecoder)
  |]
mkFieldDecoder _ a =
    error $ "mkFieldDecoder - unmatched pattern: " ++ show a

-- TODO: This recursion stop gimmmick only works if the type is directly recursive.
-- NOT if there is a cycle among decoders.
-- We should:
-- - Switch all decoder invocations to lazy as a MVP.
-- - Keep track of all types referend by a type and only insert lazy invocations where needed.
--   The performance hit may not be worth it.
mkTypeDecoder :: (TypeName, Maybe ModuleName) -> TyRef -> Text
mkTypeDecoder recursionStop@(tName, mName) TyRef{..} =
    case tyCon of
        TyVar varName -> "d" <> Text.tail varName -- TODO: Identify vars as ints, not strings.
        TyMapping mapping -> renderMapping tyArgs mapping
  where
    renderMapping :: [TyRef] -> ElmMapping -> Text
    renderMapping args mapping =
        let renderedArgs =
                Text.intercalate " " $
                    (renderMapping [] <$> mapping.args)
                        <> (mkTypeDecoder recursionStop <$> args)
         in if mapping.moduleName == mName && mapping.typeName == tName
                then -- Same module, same type: lazy decoding without module prefix.
                    "(Json.Decode.lazy (\\_ -> decode" <> tName <> " " <> renderedArgs <> "))"
                else
                    let decoderLocation = case mapping.decoderLocation of
                            Nothing -> error "No decoder"
                            Just location -> location
                        decodeFunction = (if mName == Just decoderLocation.symbolModuleName then (decoderLocation.symbolName) else decoderLocation.symbolModuleName <> "." <> decoderLocation.symbolName)
                     in case renderedArgs of
                            "" -> decodeFunction
                            _ -> "(" <> decodeFunction <> " " <> renderedArgs <> ")"

mkFieldsFunction :: [ElmField] -> Text
mkFieldsFunction fields =
    let fieldNames = (fromJust . fst) <$> fields
        args = Text.intercalate " " fieldNames
        fieldSetters = Text.intercalate "\n, " $ (\n -> n <> " = " <> n) <$> fieldNames
     in [trimming|
    \$args ->
      { $fieldSetters
      }
  |]

decodeAnonymousConstructor :: (TypeName, Maybe ModuleName) -> ConstructorName -> [ElmField] -> Text
decodeAnonymousConstructor recursionStop cName fields =
    let contentDecoder = case length fields of
            0 -> error "decodeAnonymous constructor should not be used for nullary constructors"
            1 -> "Json.Decode.map " <> cName <> mkTypeDecoder recursionStop (snd $ head fields)
            _ ->
                let numberedFields = zip @Integer [0 ..] (snd <$> fields)
                    mkFieldDecoder' (n, tyRef) =
                        "|> andMap (Json.Decode.index "
                            <> Text.pack (show n)
                            <> " "
                            <> mkTypeDecoder recursionStop tyRef
                            <> ")"
                    fieldDecoders = Text.intercalate "\n" $ mkFieldDecoder' <$> numberedFields
                 in [trimming|
            Json.Decode.succeed $cName
              $fieldDecoders
          |]
     in [trimming|
    let contentDecoder =
          $contentDecoder
    in
    Json.Decode.field "contents" contentDecoder
  |]

-- | Generate an encoder
generateEncoder :: forall {k} (x :: k). (HasGenerationSpec k x) => Text
generateEncoder = generateEncoder' $ getGenerationSpec @x

getEncoderLocation :: GenerationSpec x -> SymbolLocation
getEncoderLocation GenerationSpec{..} =
    case mapping.encoderLocation of
        Nothing -> error $ "No encoder location for: " <> show mapping.typeName
        Just location -> location

getEncoderName :: GenerationSpec x -> Text
getEncoderName = (.symbolName) . getEncoderLocation

getEncoderModule :: GenerationSpec x -> Text
getEncoderModule = (.symbolModuleName) . getEncoderLocation

generateEncoder' :: GenerationSpec x -> Text
generateEncoder' structure@(GenerationSpec{..})
    | mapping.isTypeAlias =
        [trimming|
    $encoderName : $encoderType
    $defLineAlias
      $bodyAlias
  |]
    | otherwise =
        [trimming|
    $encoderName : $encoderType
    $defLine
      $body
  |]
  where
    encoderName = structure & getEncoderName
    encoderType = mkEncoderType structure
    defLine = mkEncoderDefLine structure
    defLineAlias = mkEncoderDefLineAlias structure
    body = mkEncoderBody structure
    bodyAlias = mkEncoderBodyAlias structure

mkEncoderType :: GenerationSpec x -> Text
mkEncoderType structure =
    Text.intercalate " " $
        mconcat
            [ paramEncoderTypes
            , [qualifiedTypeNameAt structure (structure & getEncoderLocation), "->", "Value"]
            ]
  where
    paramEncoderTypes =
        ((\n -> "(e" <> n <> " -> Value) ->") . (Text.pack . show @Integer))
            <$> take (fromIntegral structure.nParams) [0 ..]

mkEncoderDefLine :: GenerationSpec x -> Text
mkEncoderDefLine structure =
    Text.intercalate " " $
        mconcat
            [ [getEncoderName structure]
            , (("e" <>) . Text.pack . show @Integer) <$> take (fromIntegral structure.nParams) [0 ..]
            , ["v", "="]
            ]

mkEncoderDefLineAlias :: GenerationSpec x -> Text
mkEncoderDefLineAlias structure =
    Text.intercalate " " $
        mconcat
            [ [getEncoderName structure]
            , (("e" <>) . Text.pack . show @Integer) <$> take (fromIntegral structure.nParams) [0 ..]
            , ["r", "="]
            ]

qualifiedTypeNameAt :: GenerationSpec x -> SymbolLocation -> Text
qualifiedTypeNameAt s@(GenerationSpec{..}) loc =
    let tName = case mapping.moduleName of
            Nothing -> mapping.typeName
            Just _ -> prefixFor s loc <> mapping.typeName
        tArgs = (("e" <>) . Text.pack . show @Integer) <$> take (fromIntegral s.nParams) [0 ..]
        allComponents = tName : tArgs
     in case allComponents of
            [single] -> single
            _multiple -> "(" <> Text.intercalate " " allComponents <> ")"

prefixFor :: GenerationSpec x -> SymbolLocation -> Text
prefixFor GenerationSpec{..} loc =
    case (mapping.moduleName, loc.symbolModuleName) of
        (Nothing, _) -> ""
        (Just m1, m2) -> if m1 == m2 then "" else m1 <> "."

mkEncoderBody :: GenerationSpec x -> Text
mkEncoderBody structure@(GenerationSpec{..}) =
    [trimming|
    case v of
      $constructorBranches
  |]
  where
    constructorBranches = case constructors of
        [] ->
            error $
                "Cannot generate encoder body (no constructors): "
                    <> show structure.mapping.typeName
        [singleConstructor] ->
            unwrapConstructorBranch structure singleConstructor
        multiple ->
            multipleConstructorBranches structure multiple

mkEncoderBodyAlias :: GenerationSpec x -> Text
mkEncoderBodyAlias structure@(GenerationSpec{..}) =
    [trimming|
    $constructorBranches
  |]
  where
    constructorBranches = case constructors of
        [] ->
            error $
                "Cannot generate encoder body (no constructors): "
                    <> show structure.mapping.typeName
        [singleConstructor] ->
            let (_, _, contentEncoder) = constructorBranchHelper structure singleConstructor
             in -- Single constructor with no field is encoded as an empty list
                fromMaybe "Json.Encode.list identity []" contentEncoder
        _ ->
            error $
                "Cannot generate encoder body as type alias (too many constructors): "
                    <> show structure.mapping.typeName

unwrapConstructorBranch :: GenerationSpec x -> Constructor -> Text
unwrapConstructorBranch s@(GenerationSpec{}) c =
    let (cName, cArgs, contentEncoder) = constructorBranchHelper s c
        matchLine = Text.intercalate " " (cName : cArgs)
     in foldMap
            ( \e ->
                [trimming|
                       $matchLine ->
                         $e
                       |]
            )
            contentEncoder

constructorBranchHelper :: GenerationSpec x -> Constructor -> (Text, [Text], Maybe Text)
constructorBranchHelper s@(GenerationSpec{}) c =
    (cName, cArgs, contentEncoder)
  where
    cName = prefixFor s (s & getEncoderLocation) <> c.constructorName
    nFields = length c.constructorFields
    contentEncoder = case c.constructorFields of
        [] ->
            -- Single constructor with no field has no content field
            Nothing
        [(Nothing, tyRef)] ->
            -- Newtype-like: we directly encode the value
            Just $ encoderForType s tyRef <> " " <> "p0"
        ((Nothing, _tyRef) : _) ->
            -- Multiple anonymous fields: encode positionally, as a list.
            let encodedValues =
                    Text.intercalate "\n, " $
                        (\(n, (_, tyRef)) -> encoderForType s tyRef <> " p" <> Text.pack (show n))
                            <$> (zip @Integer [0 ..] c.constructorFields)
             in Just
                    [trimming|
          Json.Encode.list identity
            [ $encodedValues
            ]
        |]
        recordFields ->
            let encodedPairs =
                    Text.intercalate "\n, " $
                        ( \case
                            (Just fieldName, tyRef) -> encodePair fieldName tyRef
                            a -> error $ "unmatched pattern in constructorBranchHelper: " ++ show a
                        )
                            <$> recordFields
                encodePair fieldName tyRef =
                    "(\""
                        <> fieldName
                        <> "\", "
                        <> encoderForType s tyRef
                        <> " r."
                        <> fieldName
                        <> ")"
             in Just
                    [trimming|
          Json.Encode.object
            [ $encodedPairs
            ]
        |]
    cArgs = case c.constructorFields of
        [] -> []
        ((Just _, _) : _) -> ["r"]
        _ ->
            -- Anonymous fields
            (("p" <>) . Text.pack . show @Integer) <$> take nFields [0 ..]

multipleConstructorBranches :: GenerationSpec x -> [Constructor] -> Text
multipleConstructorBranches structure constructors =
    let prefix = (prefixFor structure (structure & getEncoderLocation))
     in (if all isNullary constructors then encodeStringTags prefix ((.constructorName) <$> constructors) else encodeTaggedBranches structure constructors)

encodeStringTags :: Text -> [ConstructorName] -> Text
encodeStringTags prefix cnames =
    Text.intercalate "\n" $
        (\cname -> prefix <> cname <> " -> Json.Encode.string \"" <> cname <> "\"") <$> cnames

encodeTaggedBranches :: GenerationSpec x -> [Constructor] -> Text
encodeTaggedBranches ds cs = Text.intercalate "\n" $ mkTagBranch <$> cs
  where
    mkTagBranch c =
        let (cName, cArgs, encodedContent) =
                constructorBranchHelper ds c
            match = Text.intercalate " " (cName : cArgs)
            tag = c.constructorName
            contentsField =
                foldMap
                    ( \val ->
                        [untrimming|
                           , ( "contents"
                             , $val
                             )
                           |]
                    )
                    encodedContent
         in [trimming|
            $match -> Json.Encode.object
              [ ( "tag", Json.Encode.string "$tag" )$contentsField]
          |]

encoderForType :: GenerationSpec x -> TyRef -> Text
encoderForType ds TyRef{..} =
    case tyCon of
        TyVar varName -> "e" <> Text.tail varName -- Won't have args (No HKTs)
        TyMapping mapping -> renderMapping mapping
  where
    renderMapping mapping =
        let encoderName = case mapping.encoderLocation of
                Nothing -> error $ "Could not find encoder for type: " <> show mapping.typeName
                Just location ->
                    (if location.symbolModuleName == getEncoderModule ds then (location.symbolName) else location.symbolModuleName <> "." <> location.symbolName)
            paramEncoders =
                (renderMapping <$> mapping.args)
                    <> (encoderForType ds <$> tyArgs)
         in "(" <> Text.intercalate " " (encoderName : paramEncoders) <> ")"

--
data SomeGenerationSpec = forall x. SomeGenerationSpec (GenerationSpec x)
include :: forall {k} x. (HasGenerationSpec k x) => SomeGenerationSpec
include = SomeGenerationSpec $ getGenerationSpec @x

data ModuleDefinition = ModuleDefinition
    { imports :: Set ModuleName
    , typeDefs :: [Text]
    , encoders :: [Text]
    , decoders :: [Text]
    }
    deriving (Eq, Show)

instance Semigroup ModuleDefinition where
    m1 <> m2 =
        ModuleDefinition
            { imports = Set.union m1.imports m2.imports
            , typeDefs = m1.typeDefs <> m2.typeDefs
            , encoders = m1.encoders <> m2.encoders
            , decoders = m1.decoders <> m2.decoders
            }
instance Monoid ModuleDefinition where
    mempty = ModuleDefinition mempty mempty mempty mempty

generateAll :: FilePath -> [SomeGenerationSpec] -> IO ()
generateAll baseDir ds = do
    let srcMap = mkSourceMap ds
    traverse_ (outputModule baseDir) $ Map.toAscList srcMap

mkSourceMap :: [SomeGenerationSpec] -> Map ModuleName Text
mkSourceMap ds = Map.mapWithKey renderModule $ computeAll ds

outputModule :: FilePath -> (ModuleName, Text) -> IO ()
outputModule baseDir (mName, source) = do
    let fileDir = baseDir </> relDir
        filePath = fileDir </> fileName
        fileName = fileComponent <> ".elm"
        relDir = foldl' (</>) "" dirComponents
        fileComponent = head reversed
        dirComponents = reverse $ drop 1 reversed
        reversed = reverse moduleComponents
        moduleComponents = Text.unpack <$> Text.split (== '.') mName
    createDirectoryIfMissing True fileDir
    Text.writeFile filePath source

renderModule :: ModuleName -> ModuleDefinition -> Text
renderModule mName ModuleDefinition{..} =
    [trimming|
    module $mName exposing (..)

    $importsSrc
    import Json.Encode
    import Json.Encode exposing (Value)
    import Json.Decode
    import Json.Decode exposing (Decoder)
    import Json.Decode.Extra exposing (andMap)


    $typeDefsSrc

    $encodersSrc

    $decodersSrc
  |]
  where
    typeDefsSrc = Text.intercalate "\n\n" typeDefs
    encodersSrc = Text.intercalate "\n\n" encoders
    decodersSrc = Text.intercalate "\n\n" decoders
    importsSrc = Text.unlines $ ("import " <>) <$> (Set.toAscList imports)

computeAll :: [SomeGenerationSpec] -> Map ModuleName ModuleDefinition
computeAll = foldl' addToModules Map.empty . mconcat . fmap mkModuleDefs
  where
    addToModules modules (mName, def) =
        Map.insertWith (<>) mName def modules
    mkModuleDefs (SomeGenerationSpec ds) =
        let constructorImports = Set.fromList $ mconcat $ getImports <$> ds.constructors
            typeImport = Set.fromList $ toList ds.mapping.moduleName
         in catMaybes
                [ case ds.mapping.moduleName of
                    Nothing -> Nothing
                    Just mName -> Just (mName, mDef)
                      where
                        mDef =
                            mempty
                                { imports = Set.delete mName constructorImports
                                , typeDefs = pure $ generateTypeDef' ds
                                }
                , case ds.mapping.encoderLocation of
                    Nothing -> Nothing
                    Just encoder -> Just (encoder.symbolModuleName, mDef)
                      where
                        mDef =
                            mempty
                                { imports =
                                    Set.delete
                                        encoder.symbolModuleName
                                        (constructorImports `Set.union` typeImport)
                                , encoders = pure $ generateEncoder' ds
                                }
                , case ds.mapping.decoderLocation of
                    Nothing -> Nothing
                    Just decoder -> Just (decoder.symbolModuleName, mDef)
                      where
                        mDef =
                            mempty
                                { imports =
                                    Set.delete
                                        decoder.symbolModuleName
                                        (constructorImports `Set.union` typeImport)
                                , decoders = pure $ generateDecoder' ds
                                }
                ]

getImports :: Constructor -> [Text]
getImports Constructor{..} = mconcat $ getFieldImports <$> constructorFields
  where
    getFieldImports (_, tyRef) = getTypeImports tyRef
    getTypeImports :: TyRef -> [Text]
    getTypeImports tyRef =
        mconcat
            [ (mconcat $ getTypeImports <$> tyRef.tyArgs)
            , mappingArgs tyRef.tyCon
            ]
    mappingArgs tyCon = case tyCon of
        TyMapping mapping ->
            toList mapping.moduleName
                <> toList ((.symbolModuleName) <$> mapping.encoderLocation)
                <> toList ((.symbolModuleName) <$> mapping.decoderLocation)
                <> mconcat (mappingArgs . TyMapping <$> mapping.args)
        _ -> mempty

-- Utility functions
renderTyRef :: ModuleName -> TyRef -> Text
renderTyRef currentModule tyRef =
    wrapIfNeeded allRendered
  where
    wrapIfNeeded :: [Text] -> Text
    wrapIfNeeded xs = case xs of
        [single] -> single
        _multiple -> "(" <> Text.intercalate " " xs <> ")"

    allRendered :: [Text]
    allRendered =
        renderTyCon tyRef.tyCon <> (renderTyRef currentModule <$> tyRef.tyArgs)

    renderTyCon :: TyCon -> [Text]
    renderTyCon tyCon = case tyCon of
        TyVar v -> [v]
        TyMapping mapping -> renderMapping mapping

    mkTyCon :: ElmMapping -> Text
    mkTyCon mapping = case mapping.moduleName of
        Nothing -> mapping.typeName
        Just mName -> (if mName == currentModule then (mapping.typeName) else mName <> "." <> mapping.typeName)

    renderMapping :: ElmMapping -> [Text]
    renderMapping mapping =
        (mkTyCon mapping)
            : ((wrapIfNeeded . renderMapping) <$> mapping.args)
