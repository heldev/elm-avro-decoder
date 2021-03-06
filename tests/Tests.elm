module Tests exposing (..)

import Array exposing (Array)
import AvDe
import Avro exposing (..)
import Bytes exposing (Bytes)
import Bytes.Encode as E
import Dict exposing (Dict)
import Expect
import NTree exposing (..)
import Schema exposing (..)
import Test exposing (..)


primitiveSchema : Test
primitiveSchema =
    Test.describe "End-to-end test"
        [ test "Can decode strings" <|
            \_ ->
                parse """ "string" """
                    |> Result.andThen (\schema -> Avro.decode schema <| E.encode (E.sequence [ E.unsignedInt8 10, E.string "Hello" ]))
                    |> Expect.equal (Ok <| Leaf <| Avro.DString "Hello")
        ]


schemaDefinition : Test
schemaDefinition =
    test "Parse schema definitions" <|
        \_ ->
            let
                definition =
                    parse userSchemaJson
                        |> Result.map .definition
            in
            Expect.equal (Ok userSchemaDefinition) definition


typeIndex : Test
typeIndex =
    test "Index named types" <|
        \_ ->
            let
                types =
                    parse userSchemaJson
                        |> Result.map .types
            in
            Expect.equal (Ok userSchemaTypes) types


dataDecoding : Test
dataDecoding =
    test "Decode binary data" <|
        let
            dataTree =
                parse userSchemaJson
                    |> Result.andThen (\schema -> Avro.decode schema userAvro)
        in
        \_ ->
            Expect.equal (Ok userData) dataTree


userSchemaJson =
    """
    {
        "name": "User",
        "type": "record",
        "fields": [
            {"name": "age", "type": "int"},
            {"name": "height", "type": "float"},
            {"name": "weight", "type": "double"},
            {"name": "active", "type": "boolean"},
            {"name": "balance", "type": "long"},

            {
                "name": "preferredTheme",
                "type": {
                    "name": "Theme",
                    "type": "enum",
                    "symbols": ["Dark", "Light"]
                }
            },

            {
                "name": "fullName",
                "type": {
                    "name": "FullName",
                    "type": "record",
                    "fields": [
                        {"name": "first", "type": "string"},
                        {"name": "last", "type": "string"}
                    ]
                }
            },

            {
                "name": "friends",
                "type": {
                    "name": "LList",
                    "type": "record",
                    "fields": [
                        {"name": "value", "type": "FullName"},
                        {"name": "next", "type": ["null", "LList"]}
                    ]
                }
            },

            {
                "name": "fingerprint",
                "type": {
                    "name": "Hash",
                    "type": "fixed",
                    "size": 16
                }
            },

            {
                "name": "technologies",
                "type": {
                    "type": "array",
                    "items": "string"
                }
            },

            {
                "name": "experience",
                "type": {
                    "type": "map",
                    "values": ["string", "int", "FullName"]
                }
            }
        ]
    }
    """


userSchemaDefinition =
    (SRecord << Record "User") <|
        Tree
            [ ( "age", Leaf <| SReference <| Standard PInt )
            , ( "height", Leaf <| SReference <| Standard PFloat )
            , ( "weight", Leaf <| SReference <| Standard PDouble )
            , ( "active", Leaf <| SReference <| Standard PBoolean )
            , ( "balance", Leaf <| SReference <| Standard PLong )
            , ( "preferredTheme"
              , (Leaf << SEnum << Enum "Theme") <|
                    Array.fromList [ "Dark", "Light" ]
              )
            , ( "fullName"
              , (Leaf << SRecord << Record "FullName") <|
                    Tree
                        [ ( "first", Leaf <| SReference <| Standard PString )
                        , ( "last", Leaf <| SReference <| Standard PString )
                        ]
              )
            , ( "friends"
              , (Leaf << SRecord << Record "LList") <|
                    Tree
                        [ ( "value", Leaf <| SReference <| Custom "FullName" )
                        , ( "next"
                          , (Leaf << SUnion) <|
                                Array.fromList
                                    [ SReference <| Standard PNull
                                    , SReference <| Custom "LList"
                                    ]
                          )
                        ]
              )
            , ( "fingerprint", Leaf <| SFixed <| Fixed "Hash" 16 )
            , ( "technologies", Leaf <| SArray <| SReference <| Standard PString )
            , ( "experience"
              , (Leaf << SMap << SUnion) <|
                    Array.fromList
                        [ SReference <| Standard PString
                        , SReference <| Standard PInt
                        , SReference <| Custom "FullName"
                        ]
              )
            ]


userSchemaTypes : Index
userSchemaTypes =
    Dict.fromList
        [ ( "User", userSchemaDefinition )
        , ( "Theme"
          , (SEnum << Enum "Theme") <|
                Array.fromList [ "Dark", "Light" ]
          )
        , ( "FullName"
          , (SRecord << Record "FullName") <|
                Tree
                    [ ( "first", Leaf <| SReference <| Standard PString )
                    , ( "last", Leaf <| SReference <| Standard PString )
                    ]
          )
        , ( "LList"
          , (SRecord << Record "LList") <|
                Tree
                    [ ( "value", Leaf <| SReference <| Custom "FullName" )
                    , ( "next"
                      , (Leaf << SUnion) <|
                            Array.fromList
                                [ SReference <| Standard PNull
                                , SReference <| Custom "LList"
                                ]
                      )
                    ]
          )
        , ( "Hash", SFixed <| Fixed "Hash" 16 )
        ]


userAvro : Bytes
userAvro =
    E.encode <|
        E.sequence
            [ E.unsignedInt8 0x2A
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0xBC
            , E.unsignedInt8 0x40
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x90
            , E.unsignedInt8 0x66
            , E.unsignedInt8 0x40
            , E.unsignedInt8 0x01
            , E.unsignedInt8 0xFE
            , E.unsignedInt8 0xC8
            , E.unsignedInt8 0xE9
            , E.unsignedInt8 0x07
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x0C
            , E.unsignedInt8 0x4A
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x61
            , E.unsignedInt8 0x6E
            , E.unsignedInt8 0x6E
            , E.unsignedInt8 0x61
            , E.unsignedInt8 0x06
            , E.unsignedInt8 0x44
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x08
            , E.unsignedInt8 0x4A
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x68
            , E.unsignedInt8 0x6E
            , E.unsignedInt8 0x06
            , E.unsignedInt8 0x44
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x02
            , E.unsignedInt8 0x06
            , E.unsignedInt8 0x42
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x62
            , E.unsignedInt8 0x0C
            , E.unsignedInt8 0x43
            , E.unsignedInt8 0x72
            , E.unsignedInt8 0x79
            , E.unsignedInt8 0x70
            , E.unsignedInt8 0x74
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x00
            , fingerprintEncoder
            , E.unsignedInt8 0x04
            , E.unsignedInt8 0x06
            , E.unsignedInt8 0x45
            , E.unsignedInt8 0x6C
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x0C
            , E.unsignedInt8 0x4B
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x74
            , E.unsignedInt8 0x6C
            , E.unsignedInt8 0x69
            , E.unsignedInt8 0x6E
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x06
            , E.unsignedInt8 0x14
            , E.unsignedInt8 0x73
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x4E
            , E.unsignedInt8 0x75
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x62
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x72
            , E.unsignedInt8 0x02
            , E.unsignedInt8 0x9C
            , E.unsignedInt8 0x01
            , E.unsignedInt8 0x10
            , E.unsignedInt8 0x73
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x4E
            , E.unsignedInt8 0x61
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x04
            , E.unsignedInt8 0x0A
            , E.unsignedInt8 0x41
            , E.unsignedInt8 0x6C
            , E.unsignedInt8 0x69
            , E.unsignedInt8 0x63
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x0C
            , E.unsignedInt8 0x43
            , E.unsignedInt8 0x72
            , E.unsignedInt8 0x79
            , E.unsignedInt8 0x70
            , E.unsignedInt8 0x74
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x14
            , E.unsignedInt8 0x73
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x53
            , E.unsignedInt8 0x74
            , E.unsignedInt8 0x72
            , E.unsignedInt8 0x69
            , E.unsignedInt8 0x6E
            , E.unsignedInt8 0x67
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x12
            , E.unsignedInt8 0x73
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0x6D
            , E.unsignedInt8 0x65
            , E.unsignedInt8 0x74
            , E.unsignedInt8 0x68
            , E.unsignedInt8 0x69
            , E.unsignedInt8 0x6E
            , E.unsignedInt8 0x67
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0xE0
            , E.unsignedInt8 0x7F
            , E.unsignedInt8 0xF1
            , E.unsignedInt8 0xCA
            , E.unsignedInt8 0x5B
            , E.unsignedInt8 0xE8
            , E.unsignedInt8 0xF4
            , E.unsignedInt8 0x87
            , E.unsignedInt8 0xAC
            , E.unsignedInt8 0xB2
            , E.unsignedInt8 0x4A
            , E.unsignedInt8 0x6F
            , E.unsignedInt8 0xDD
            , E.unsignedInt8 0xDE
            , E.unsignedInt8 0xFF
            , E.unsignedInt8 0xB9
            ]


fingerprintEncoder : E.Encoder
fingerprintEncoder =
    E.sequence
        [ E.unsignedInt8 0x00
        , E.unsignedInt8 0x0A
        , E.unsignedInt8 0xFA
        , E.unsignedInt8 0xFF
        , E.unsignedInt8 0xEE
        , E.unsignedInt8 0x11
        , E.unsignedInt8 0xEA
        , E.unsignedInt8 0xAE
        , E.unsignedInt8 0x00
        , E.unsignedInt8 0x0A
        , E.unsignedInt8 0xFA
        , E.unsignedInt8 0xFF
        , E.unsignedInt8 0xEE
        , E.unsignedInt8 0x11
        , E.unsignedInt8 0xEA
        , E.unsignedInt8 0xAE
        ]


userData : Tree Datum
userData =
    Tree
        [ ( "age", Leaf <| DInt 21 )
        , ( "height", Leaf <| DFloat 5.875 )
        , ( "weight", Leaf <| DFloat 180.5 )
        , ( "active", Leaf <| DBool True )
        , ( "balance", Leaf <| DInt 8204863 )
        , ( "preferredTheme", Leaf <| DString "Dark" )
        , ( "fullName"
          , Tree
                [ ( "first", Leaf <| DString "Joanna" )
                , ( "last", Leaf <| DString "Doe" )
                ]
          )
        , ( "friends"
          , Tree
                [ ( "value"
                  , Tree
                        [ ( "first", Leaf <| DString "John" )
                        , ( "last", Leaf <| DString "Doe" )
                        ]
                  )
                , ( "next"
                  , Tree
                        [ ( "value"
                          , Tree
                                [ ( "first", Leaf <| DString "Bob" )
                                , ( "last", Leaf <| DString "Crypto" )
                                ]
                          )
                        , ( "next", Leaf DNull )
                        ]
                  )
                ]
          )
        , ( "fingerprint", Leaf <| DBytes <| E.encode fingerprintEncoder )
        , ( "technologies"
          , (Leaf << DArray) <|
                Array.fromList
                    [ Leaf <| DString "Elm"
                    , Leaf <| DString "Kotlin"
                    ]
          )
        , ( "experience"
          , Tree
                [ ( "someNumber", Leaf <| DInt 78 )
                , ( "someName"
                  , Tree
                        [ ( "first", Leaf <| DString "Alice" )
                        , ( "last", Leaf <| DString "Crypto" )
                        ]
                  )
                , ( "someString", Leaf <| DString "something" )
                ]
          )
        ]


type alias User =
    { age : Int
    , height : Float
    , weight : Float
    , active : Bool
    , balance : Int
    , preferredTheme : Theme
    , fullName : FullName
    , friends : LList FullName
    , fingerprint : Bytes
    , technologies : Array String
    , experience : Dict String ExperienceValue
    }


decoders : Test
decoders =
    test "Decode complex type" <|
        \_ ->
            AvDe.decode AvDe.value userData
                |> Result.andThen (AvDe.decode userDecoder)
                |> Expect.equal (Ok user)


user : User
user =
    User 21
        5.875
        180.5
        True
        8204863
        Dark
        (FullName "Joanna" "Doe")
        (LList
            (FullName "John" "Doe")
            (LList
                (FullName "Bob" "Crypto")
                Empty
            )
        )
        (E.encode fingerprintEncoder)
        (Array.fromList [ "Elm", "Kotlin" ])
        (Dict.fromList
            [ ( "someNumber", EInt 78 )
            , ( "someName", EFullName <| FullName "Alice" "Crypto" )
            , ( "someString", EString "something" )
            ]
        )


type Theme
    = Light
    | Dark


type alias FullName =
    { first : String
    , last : String
    }


type LList a
    = LList a (LList a)
    | Empty


type ExperienceValue
    = EString String
    | EFullName FullName
    | EInt Int


userDecoder : AvDe.Decoder User
userDecoder =
    AvDe.succeed User
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "age" AvDe.int)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "height" AvDe.float)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "weight" AvDe.float)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "active" AvDe.bool)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "balance" AvDe.int)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "preferredTheme" themeDecoder)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "fullName" fullNameDecoder)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "friends" <| llistDecoder fullNameDecoder)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "fingerprint" <| AvDe.bytes)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "technologies" <| AvDe.array AvDe.string)
        |> AvDe.andThen (\c -> AvDe.map c <| AvDe.field "experience" <| AvDe.dict experienceValueDecoder)


themeDecoder : AvDe.Decoder Theme
themeDecoder =
    AvDe.string
        |> AvDe.andThen
            (\theme ->
                case theme of
                    "Light" ->
                        AvDe.succeed Light

                    "Dark" ->
                        AvDe.succeed Dark

                    _ ->
                        AvDe.fail ("Field theme contains an incorrect value " ++ theme)
            )


fullNameDecoder : AvDe.Decoder FullName
fullNameDecoder =
    AvDe.map2 FullName
        (AvDe.field "first" AvDe.string)
        (AvDe.field "last" AvDe.string)


llistDecoder : AvDe.Decoder a -> AvDe.Decoder (LList a)
llistDecoder decoder =
    AvDe.oneOf
        [ AvDe.null Empty
        , AvDe.map2 LList
            (AvDe.field "value" decoder)
            (AvDe.field "next" <| AvDe.lazy <| \() -> llistDecoder decoder)
        ]


experienceValueDecoder : AvDe.Decoder ExperienceValue
experienceValueDecoder =
    AvDe.oneOf
        [ AvDe.map EInt AvDe.int
        , AvDe.map EString AvDe.string
        , AvDe.map EFullName fullNameDecoder
        ]
