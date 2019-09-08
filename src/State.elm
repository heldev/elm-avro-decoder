module State exposing (State, andThen, put, run, state)


type State a s
    = State (s -> ( a, s ))


andThen : (a -> State b s) -> State a s -> State b s
andThen function (State step) =
    State <|
        \s ->
            let
                ( value, newState ) =
                    step s
            in
            run (function value) newState


state : a -> State a s
state value =
    State (\s -> ( value, s ))


run : State a s -> (s -> ( a, s ))
run (State step) =
    step


put : s -> State () s
put s =
    State <| \_ -> ( (), s )
