module Decoder exposing (..)

import AvDe
import Avro exposing (Datum(..))
import Bytes.Encode as E
import Expect
import Schema
import Test exposing (Test, test)


bool : Test
bool =
    test "Decode bool" <|
        \_ ->
            Schema.parse """ "boolean" """
                |> Result.andThen (\schema -> Avro.decode schema <| E.encode (E.unsignedInt8 1))
                |> Result.andThen (AvDe.decode AvDe.bool)
                |> Expect.equal (Ok True)


int : Test
int =
    test "Decode int" <|
        \_ ->
            Schema.parse """ "int" """
                |> Result.andThen (\schema -> Avro.decode schema <| E.encode (E.unsignedInt8 21))
                |> Result.andThen (AvDe.decode AvDe.int)
                |> Expect.equal (Ok -10)


type alias User =
    { id : Int, active : Bool }


field : Test
field =
    test "Decode field" <|
        \_ ->
            let
                userDecoder =
                    AvDe.map2 User
                        (AvDe.field "id" AvDe.int)
                        (AvDe.field "active" AvDe.bool)
            in
            Schema.parse """{
                "name": "User",
                "type": "record",
                "fields": [
                    {"name": "id", "type": "int"},
                    {"name": "active", "type": "boolean"}
                ]
            }"""
                |> Result.andThen (\schema -> Avro.decode schema <| E.encode (E.sequence [ E.unsignedInt8 102, E.unsignedInt8 1 ]))
                |> Result.andThen (AvDe.decode userDecoder)
                |> Expect.equal (Ok <| User 51 True)
