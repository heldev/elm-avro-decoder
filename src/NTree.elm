module NTree exposing (..)

import Bytes.Decode as D
import Debug exposing (todo)


type Tree a
    = Tree (List ( String, Tree a ))
    | Leaf a


sequenceTree : Tree (D.Decoder a) -> D.Decoder (Tree a)
sequenceTree =
    traverse identity


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
        Tree namedTrees ->
            Tree <| List.map (Tuple.mapSecond <| map f) namedTrees

        Leaf value ->
            Leaf <| f value


fold : (e -> a -> a) -> a -> Tree e -> a
fold f acc tree =
    case tree of
        Tree trees ->
            let
                listF : ( String, Tree e ) -> a -> a
                listF ( _, tr ) listAcc =
                    fold f listAcc tr
            in
            List.foldl listF acc trees

        Leaf element ->
            f element acc


traverse : (a -> D.Decoder b) -> Tree a -> D.Decoder (Tree b)
traverse f tree =
    case tree of
        Tree namedTrees ->
            let
                ( names, trees ) =
                    List.unzip namedTrees

                treeDecoders =
                    sequence <| List.map (traverse f) trees
            in
            D.map (Tree << List.map2 Tuple.pair names) treeDecoders

        Leaf decoder ->
            D.map Leaf <| f decoder


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
        Tree namedTrees ->
            let
                ( names, trees ) =
                    List.unzip namedTrees

                maybeTrees =
                    sequenceMaybe <| List.map sequenceTreeMaybe trees
            in
            Maybe.map (Tree << List.map2 Tuple.pair names) maybeTrees

        Leaf maybe ->
            Maybe.map Leaf maybe


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    let
        step maybe maybeList =
            Maybe.andThen (\v -> Maybe.map ((::) v) maybeList) maybe
    in
    List.foldl step (Just [])
