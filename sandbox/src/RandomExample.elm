module RandomExample exposing (..)

import Browser
import Html exposing (..)
import Random exposing (Generator, Seed)


type Msg
    = GotSeed Int


init () =
    ( 0, Random.generate GotSeed <| Random.int 1 100 )


view model =
    let
        initialSeed =
            Random.initialSeed model

        reducer _ ( xs, seed ) =
            Random.step (Random.int 1 100) seed |> Tuple.mapFirst (List.singleton >> List.append xs)

        list =
            List.range 1 10 |> List.foldl reducer ( [], initialSeed ) |> Tuple.first

        mapper =
            String.fromInt >> text >> List.singleton >> li []
    in
        div []
            [ h1 [] [ text "RandomExample" ]
            , ul [] (list |> List.map mapper)
            ]


update msg model =
    case msg of
        GotSeed s ->
            ( s, Cmd.none )


main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }
