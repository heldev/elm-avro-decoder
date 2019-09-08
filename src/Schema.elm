module Schema exposing (Enum, Fixed, Index, Primitive(..), Record, Reference(..), Schema, SchemaDefinition(..), parse)

import Array exposing (Array)
import Debug exposing (todo)
import Dict exposing (Dict)
import Json.Decode as D
import NTree exposing (Tree(..))


type SchemaDefinition
    = SRecord Record
    | SUnion (Array SchemaDefinition)
    | SEnum Enum
    | SArray SchemaDefinition
    | SMap SchemaDefinition
    | SFixed Fixed
    | SReference Reference


type alias Record =
    { name : Name
    , fields : Tree SchemaDefinition
    }


type alias Enum =
    { name : Name
    , elements : Array String
    }


type alias Fixed =
    { name : Name
    , size : Int
    }


type Reference
    = Standard Primitive
    | Custom Name


type Primitive
    = PNull
    | PBoolean
    | PInt
    | PLong
    | PFloat
    | PDouble
    | PBytes
    | PString


type alias Name =
    String


type alias Schema =
    { definition : SchemaDefinition
    , types : Index
    }


parse : String -> Result String Schema
parse json =
    let
        definition =
            writerSchema json
                |> Result.mapError D.errorToString

        types =
            definition |> Result.andThen indexSchema
    in
    Result.map2 Schema definition types


primitives : Dict String Primitive
primitives =
    Dict.fromList
        [ ( "null", PNull )
        , ( "boolean", PBoolean )
        , ( "int", PInt )
        , ( "long", PLong )
        , ( "float", PFloat )
        , ( "double", PDouble )
        , ( "bytes", PBytes )
        , ( "string", PString )
        ]


type alias Index =
    Dict Name SchemaDefinition


indexSchema : SchemaDefinition -> Result String Index
indexSchema schemaDefinition =
    indexSchemaHelp schemaDefinition <| Ok <| Dict.empty


indexSchemaHelp : SchemaDefinition -> Result String Index -> Result String Index
indexSchemaHelp schema index =
    case schema of
        SRecord record ->
            checkAndAdd index record schema
                |> indexTree record.fields

        SUnion schemas ->
            Array.foldl indexSchemaHelp index schemas

        SEnum enum ->
            checkAndAdd index enum schema

        SArray arraySchema ->
            indexSchemaHelp arraySchema index

        SMap mapSchema ->
            indexSchemaHelp mapSchema index

        SFixed fixed ->
            checkAndAdd index fixed schema

        SReference reference ->
            case reference of
                Custom name ->
                    index |> Result.andThen (verifyAlreadyDeclared name)

                Standard _ ->
                    index


verifyAlreadyDeclared : Name -> Index -> Result String Index
verifyAlreadyDeclared name index =
    if Dict.member name index then
        Ok index

    else
        Err <| "No custom type defined, name= " ++ name


indexTree : Tree SchemaDefinition -> Result String Index -> Result String Index
indexTree tree index =
    case tree of
        Tree children ->
            List.unzip children
                |> Tuple.second
                |> List.foldl indexTree index

        Leaf schema ->
            indexSchemaHelp schema index


type alias Named a =
    { a | name : Name }


checkAndAdd : Result String Index -> Named a -> SchemaDefinition -> Result String Index
checkAndAdd schemaIndex named schema =
    schemaIndex
        |> Result.andThen
            (\index ->
                if Dict.member named.name index then
                    Err <| "Custom type with the name '" ++ named.name ++ "' was already defined"

                else
                    Ok <| Dict.insert named.name schema index
            )


writerSchema : String -> Result D.Error SchemaDefinition
writerSchema json =
    D.decodeString schemaDecoder json


schemaDecoder : D.Decoder SchemaDefinition
schemaDecoder =
    D.oneOf [ referenceDecoder, customTypeDecoder, unionDecoder ]


referenceDecoder : D.Decoder SchemaDefinition
referenceDecoder =
    D.string
        |> D.andThen (D.succeed << toReference)
        |> D.map SReference


toReference : Name -> Reference
toReference name =
    Dict.get name primitives
        |> Maybe.map Standard
        |> Maybe.withDefault (Custom name)


customTypeDecoder : D.Decoder SchemaDefinition
customTypeDecoder =
    D.field "type" D.string
        |> D.andThen decoderByKind


decoderByKind : String -> D.Decoder SchemaDefinition
decoderByKind kind =
    let
        decoders =
            Dict.fromList
                [ ( "record", recordDecoder )
                , ( "enum", enumDecoder )
                , ( "array", arrayDecoder )
                , ( "map", mapDecoder )
                , ( "fixed", fixedDecoder )
                ]

        unknown =
            D.fail <| "Unknown complex kind 'type='" ++ kind
    in
    Dict.get kind decoders
        |> Maybe.withDefault unknown


recordDecoder : D.Decoder SchemaDefinition
recordDecoder =
    D.map SRecord <|
        D.map2 Record
            (D.field "name" D.string)
            (D.map Tree <| D.field "fields" <| D.list fieldDecoder)


fieldDecoder : D.Decoder ( String, Tree SchemaDefinition )
fieldDecoder =
    D.map2 Tuple.pair
        (D.field "name" D.string)
        (D.map Leaf <| D.field "type" schemaDecoder)


enumDecoder : D.Decoder SchemaDefinition
enumDecoder =
    D.map SEnum <|
        D.map2 Enum
            (D.field "name" D.string)
            (D.field "symbols" <| D.array D.string)


arrayDecoder : D.Decoder SchemaDefinition
arrayDecoder =
    D.map SArray <| D.field "items" schemaDecoder


mapDecoder : D.Decoder SchemaDefinition
mapDecoder =
    D.map SMap <| D.field "values" schemaDecoder


fixedDecoder : D.Decoder SchemaDefinition
fixedDecoder =
    D.map SFixed <|
        D.map2 Fixed
            (D.field "name" D.string)
            (D.field "size" D.int)


unionDecoder : D.Decoder SchemaDefinition
unionDecoder =
    D.map SUnion <| D.array <| D.lazy <| \_ -> schemaDecoder
