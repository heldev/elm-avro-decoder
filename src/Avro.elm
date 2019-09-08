module Avro exposing (..)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Debug exposing (todo)
import Dict
import NTree exposing (Tree(..))
import Schema exposing (Fixed, Schema)


decode : Schema -> Bytes -> Result String (Tree Datum)
decode schema =
    D.decode (dataDecoder schema)
        >> Result.fromMaybe "Can't decode provided bytes using that schema"


dataDecoder : Schema -> D.Decoder (Tree Datum)
dataDecoder schema =
    case schema.definition of
        Schema.SRecord record ->
            recordDecoder schema record.fields

        Schema.SUnion definitions ->
            intDecoder
                |> D.map (\definitionNumber -> Array.get definitionNumber definitions)
                |> D.map (Maybe.map (\definition -> dataDecoder { schema | definition = definition }))
                |> D.andThen (Maybe.withDefault D.fail)

        Schema.SEnum enum ->
            intDecoder
                |> D.map (\position -> Array.get position enum.elements)
                |> D.andThen (\element -> Maybe.map D.succeed element |> Maybe.withDefault D.fail)
                |> D.map (Leaf << DString)

        Schema.SArray elementDefinition ->
            multiBlock (dataDecoder { schema | definition = elementDefinition })
                |> D.map (Leaf << DArray << Array.fromList)

        Schema.SMap mapSchemaDefinition ->
            multiBlock (keyValueDecoder { schema | definition = mapSchemaDefinition })
                |> D.map Tree

        Schema.SFixed fixed ->
            D.bytes fixed.size
                |> D.map (Leaf << DBytes)

        Schema.SReference reference ->
            case reference of
                Schema.Standard primitive ->
                    primitiveDecoder primitive
                        |> D.map Leaf

                Schema.Custom name ->
                    Dict.get name schema.types
                        |> Maybe.map (\definition -> dataDecoder { schema | definition = definition })
                        |> Maybe.withDefault D.fail


recordDecoder : Schema -> Tree Schema.SchemaDefinition -> D.Decoder (Tree Datum)
recordDecoder schema tree =
    case tree of
        Tree fields ->
            let
                ( names, definitions ) =
                    List.unzip fields
            in
            List.map (\definition -> recordDecoder schema definition) definitions
                |> NTree.sequence
                |> D.map (Tree << List.map2 Tuple.pair names)

        Leaf definition ->
            dataDecoder { schema | definition = definition }


primitiveDecoder : Schema.Primitive -> D.Decoder Datum
primitiveDecoder primitive =
    case primitive of
        Schema.PNull ->
            D.succeed DNull

        Schema.PBoolean ->
            D.unsignedInt8
                |> D.map (DBool << (==) 1)

        Schema.PInt ->
            D.map DInt intDecoder

        Schema.PLong ->
            D.map DInt intDecoder

        Schema.PFloat ->
            D.map DFloat floatDecoder

        Schema.PDouble ->
            D.map DFloat doubleDecoder

        Schema.PBytes ->
            intDecoder
                |> D.andThen D.bytes
                |> D.map DBytes

        Schema.PString ->
            D.map DString stringDecoder


intDecoder : D.Decoder Int
intDecoder =
    D.loop ( 0, 0 ) numberDecoderStep
        |> D.map decodeZigZag


numberDecoderStep : ( Int, Int ) -> D.Decoder (D.Step ( Int, Int ) Int)
numberDecoderStep state =
    D.unsignedInt8
        |> D.map
            (\byte ->
                if 128 <= byte then
                    D.Loop <| prependSeptet state byte

                else
                    D.Done <| Tuple.first <| prependSeptet state byte
            )


prependSeptet : ( Int, Int ) -> Int -> ( Int, Int )
prependSeptet ( value, length ) septet =
    ( value + Bitwise.and septet 0x7F * (0x80 ^ length)
    , length + 1
    )


decodeZigZag : Int -> Int
decodeZigZag zigZag =
    if Bitwise.and zigZag 1 == 1 then
        -zigZag // 2

    else
        zigZag // 2


floatDecoder : D.Decoder Float
floatDecoder =
    D.float32 endianness


doubleDecoder : D.Decoder Float
doubleDecoder =
    D.float64 endianness


stringDecoder : D.Decoder String
stringDecoder =
    intDecoder
        |> D.andThen D.string


multiBlock : D.Decoder a -> D.Decoder (List a)
multiBlock elementDecoder =
    D.loop [] <| multiBlockStep elementDecoder


multiBlockStep :
    D.Decoder a
    -> List a
    -> D.Decoder (D.Step (List a) (List a))
multiBlockStep elementDecoder elements =
    intDecoder
        |> D.andThen
            (\blockSize ->
                if blockSize == 0 then
                    D.succeed <| D.Done elements

                else
                    block elementDecoder blockSize
                        |> D.map (D.Loop << List.append elements)
            )


multiBlockB : D.Decoder a -> D.Decoder (List a)
multiBlockB elementDecoder =
    D.loop [] (multiBlockStepB (list elementDecoder) [])
        |> D.map List.concat


multiBlockStepB :
    D.Decoder a
    -> a
    -> List a
    -> D.Decoder (D.Step (List a) (List a))
multiBlockStepB elementDecoder stop elements =
    elementDecoder
        |> D.map
            (\element ->
                if element == stop then
                    D.Done <| List.reverse elements

                else
                    D.Loop (element :: elements)
            )


multiBlockC : D.Decoder a -> D.Decoder (List a)
multiBlockC elementDecoder =
    D.loop [] <| multiBlockStepC elementDecoder


multiBlockStepC :
    D.Decoder a
    -> List (List a)
    -> D.Decoder (D.Step (List (List a)) (List a))
multiBlockStepC elementDecoder blocks =
    intDecoder
        |> D.andThen
            (\blockSize ->
                if blockSize == 0 then
                    D.succeed <| D.Done <| List.concat <| List.map List.reverse <| blocks

                else
                    block elementDecoder blockSize
                        |> D.map (\b -> D.Loop (b :: blocks))
            )


list : D.Decoder a -> D.Decoder (List a)
list elementDecoder =
    D.andThen (block elementDecoder) intDecoder


block : D.Decoder a -> Int -> D.Decoder (List a)
block element count =
    D.loop ( count, [] ) <| blockStep element


blockStep :
    D.Decoder a
    -> ( Int, List a )
    -> D.Decoder (D.Step ( Int, List a ) (List a))
blockStep elementDecoder ( count, elements ) =
    if count == 0 then
        D.succeed <| D.Done <| List.reverse elements

    else
        elementDecoder |> D.map (\element -> D.Loop ( count - 1, element :: elements ))


keyValueDecoder : Schema -> D.Decoder ( String, Tree Datum )
keyValueDecoder valueSchema =
    D.map2 Tuple.pair
        stringDecoder
        (dataDecoder valueSchema)


endianness : Endianness
endianness =
    LE


type Datum
    = DNull
    | DBool Bool
    | DInt Int
    | DFloat Float
    | DBytes Bytes
    | DString String
    | DArray (Array (Tree Datum))
