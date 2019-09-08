module Tree exposing (..)

import Bytes.Decode as D


type Tree a
    = Node (List (Tree a))
    | Leaf a


sequenceTree : Tree (D.Decoder a) -> D.Decoder (Tree a)
sequenceTree =
    traverse identity


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f tree =
    case tree of
        Node trees ->
            Node <| List.map (mapTree f) trees

        Leaf a ->
            Leaf << f <| a


traverse : (a -> D.Decoder b) -> Tree a -> D.Decoder (Tree b)
traverse f tree =
    case tree of
        Node trees ->
            let
                listOfDecoders =
                    List.map (traverse f) trees
            in
            D.map Node (sequence listOfDecoders)

        Leaf value ->
            D.map Leaf <| f value


sequence : List (D.Decoder a) -> D.Decoder (List a)
sequence =
    let
        step elementDecoder listDecoder =
            elementDecoder |> D.andThen (\element -> D.map ((::) element) listDecoder)
    in
    List.reverse >> List.foldl step (D.succeed [])
