module AvDe exposing
    ( Decoder
    , andThen
    , array
    , bool
    , bytes
    , decode
    , dict
    , fail
    , field
    , float
    , int
    , lazy
    , list
    , map
    , map2
    , null
    , oneOf
    , string
    , succeed
    , value
    )

import Array exposing (Array)
import Avro exposing (Datum(..))
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import NTree exposing (Tree(..))


decode : Decoder a -> Tree Datum -> Result String a
decode (Decoder decoder) data =
    decoder data


type Decoder a
    = Decoder (Tree Datum -> Result String a)


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen function (Decoder decoder) =
    Decoder <|
        \data ->
            decoder data
                |> Result.map (run << function)
                |> Result.andThen (\f -> f data)


run : Decoder a -> (Tree Datum -> Result String a)
run (Decoder decoder) =
    decoder


map : (a -> b) -> Decoder a -> Decoder b
map function (Decoder decoder) =
    Decoder <|
        \data ->
            Result.map function <| decoder data


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 function (Decoder decoderA) (Decoder decoderB) =
    Decoder <|
        \data ->
            Result.map2 function
                (decoderA data)
                (decoderB data)


null : a -> Decoder a
null a =
    Decoder <|
        \data ->
            case data of
                Leaf DNull ->
                    Ok a

                _ ->
                    Err "Value is not null"


bool : Decoder Bool
bool =
    Decoder <|
        \data ->
            case data of
                Leaf (DBool boolValue) ->
                    Ok boolValue

                _ ->
                    Err "Can't extract bool"


int : Decoder Int
int =
    Decoder <|
        \data ->
            case data of
                Leaf (DInt intValue) ->
                    Ok intValue

                _ ->
                    Err "Can't extract int"


float : Decoder Float
float =
    Decoder <|
        \data ->
            case data of
                Leaf (DFloat floatValue) ->
                    Ok floatValue

                _ ->
                    Err "Can't extract float"


string : Decoder String
string =
    Decoder <|
        \data ->
            case data of
                Leaf (DString stringValue) ->
                    Ok stringValue

                _ ->
                    Err "Can't extract string"


succeed : a -> Decoder a
succeed a =
    Decoder <| \_ -> Ok a


fail : String -> Decoder a
fail error =
    Decoder <| \_ -> Err error


bytes : Decoder Bytes
bytes =
    Decoder <|
        \data ->
            case data of
                Leaf (DBytes bytesValue) ->
                    Ok bytesValue

                _ ->
                    Err "Can't extract bytes"


field : String -> Decoder a -> Decoder a
field fieldName (Decoder decoder) =
    Decoder <|
        \data ->
            case data of
                Tree fields ->
                    List.filter ((==) fieldName << Tuple.first) fields
                        |> List.head
                        |> Result.fromMaybe ("No field named " ++ fieldName)
                        |> Result.andThen (decoder << Tuple.second)

                Leaf _ ->
                    Err "Structure has no fields"


array : Decoder a -> Decoder (Array a)
array decoder =
    list decoder
        |> map Array.fromList


list : Decoder a -> Decoder (List a)
list (Decoder decoder) =
    Decoder <|
        \data ->
            case data of
                Leaf (DArray elements) ->
                    List.map decoder (Array.toList elements)
                        |> sequence

                _ ->
                    Err "Can't extract array"


sequence : List (Result String a) -> Result String (List a)
sequence results =
    List.foldr
        (\result resultList ->
            Result.andThen (\a -> Result.map ((::) a) resultList) result
        )
        (Ok [])
        results


dict : Decoder a -> Decoder (Dict String a)
dict (Decoder decoder) =
    Decoder <|
        \data ->
            case data of
                Tree fields ->
                    List.map (\( name, datum ) -> Result.map (Tuple.pair name) (decoder datum)) fields
                        |> sequence
                        |> Result.map Dict.fromList

                _ ->
                    Err "Can't extract dict"


value : Decoder (Tree Datum)
value =
    Decoder Ok


lazy : (() -> Decoder a) -> Decoder a
lazy function =
    succeed () |> andThen function


oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder <|
        \data ->
            List.filterMap (\(Decoder decoder) -> Result.toMaybe <| decoder data) decoders
                |> (Result.fromMaybe "No matching decoder" << List.head)
