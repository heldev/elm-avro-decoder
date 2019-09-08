module Main exposing (..)

import Avro exposing (Datum)
import Bytes exposing (Bytes)
import Bytes.Encode as E
import Html exposing (text)
import NTree
import Schema


main =
    let
        d =
            Schema.parse userSchemaJson
                |> Result.map (\schema -> Avro.decode schema <| userAvro)
    in
    text <| Debug.toString <| d


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


intAvro : Bytes
intAvro =
    E.encode <| E.sequence [ E.unsignedInt8 0x9C, E.unsignedInt8 0x01 ]


longAvro : Bytes
longAvro =
    E.encode <|
        E.sequence
            [ E.unsignedInt8 0xFE
            , E.unsignedInt8 0xC8
            , E.unsignedInt8 0xE9
            , E.unsignedInt8 0x07
            ]


userAvro : Bytes
userAvro =
    E.encode <|
        E.sequence
            [ E.unsignedInt8 0x2A
            , E.unsignedInt8 0x1F
            , E.unsignedInt8 0x85
            , E.unsignedInt8 0xA3
            , E.unsignedInt8 0x40
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x00
            , E.unsignedInt8 0x80
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
            , E.unsignedInt8 0x00
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
