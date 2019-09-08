module NamedTree exposing (..)

import Bytes.Decode as D


type Tree a
    = Tree String (List (Tree a))
    | Leaf String a


sequenceTree : Tree (D.Decoder a) -> D.Decoder (Tree a)
sequenceTree =
    traverse identity


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f tree =
    case tree of
        Tree name trees ->
            Tree name <| List.map (mapTree f) trees

        Leaf name a ->
            Leaf name << f <| a


traverse : (a -> D.Decoder b) -> Tree a -> D.Decoder (Tree b)
traverse f tree =
    case tree of
        Tree name trees ->
            let
                listOfDecoders =
                    List.map (traverse f) trees
            in
            D.map (Tree name) <| sequence listOfDecoders

        Leaf name value ->
            D.map (Leaf name) <| f value


sequence : List (D.Decoder a) -> D.Decoder (List a)
sequence =
    let
        step elementDecoder listDecoder =
            elementDecoder |> D.andThen (\element -> D.map ((::) element) listDecoder)
    in
    List.reverse >> List.foldl step (D.succeed [])


sequenceTreeMaybe : Tree (Maybe a) -> Maybe (Tree a)
sequenceTreeMaybe tree =
    case tree of
        Tree name trees ->
            let
                listOfDecoders =
                    List.map sequenceTreeMaybe trees
            in
            Maybe.map (Tree name) <| sequenceMaybe listOfDecoders

        Leaf name maybe ->
            Maybe.map (Leaf name) maybe


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    let
        step maybe maybeList =
            Maybe.andThen (\v -> Maybe.map ((::) v) maybeList) maybe
    in
    List.foldl step (Just [])
