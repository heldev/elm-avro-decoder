module AvroDecoder exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D


type AvroDecoder a
    = AvroDecoder (D.Decoder a)


stringAvroDecoder : AvroDecoder String
stringAvroDecoder =
    AvroDecoder (D.string 6)


intAvroDecoder : AvroDecoder Int
intAvroDecoder =
    AvroDecoder (D.unsignedInt32 LE)


avroDecode : AvroDecoder a -> Bytes -> Maybe a
avroDecode (AvroDecoder decoder) bytes =
    D.decode decoder bytes


avroMap2 : (a -> b -> result) -> AvroDecoder a -> AvroDecoder b -> AvroDecoder result
avroMap2 f (AvroDecoder decoderA) (AvroDecoder decoderB) =
    AvroDecoder <| D.map2 f decoderA decoderB


maybe : D.Decoder a -> D.Decoder (Maybe a)
maybe decoder =
    let
        maybeContent n =
            if n == 0 then
                D.succeed Nothing

            else
                D.map Just decoder
    in
    D.unsignedInt8 |> D.andThen maybeContent


list : D.Decoder a -> D.Decoder (List a)
list decoder =
    let
        listStep ( length, elements ) =
            if length == 0 then
                D.succeed (D.Done elements)

            else
                D.map (\e -> D.Loop ( length - 1, e :: elements )) decoder
    in
    D.unsignedInt8 |> D.andThen (\length -> D.loop ( length, [] ) listStep)
