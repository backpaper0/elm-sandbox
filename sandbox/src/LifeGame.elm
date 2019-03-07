module LifeGame exposing (..)

import Array exposing (Array)
import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Posix)
import Html exposing (Html)


type alias Matrix a =
    { cells : Array a, size : Int }


get : Int -> Int -> Matrix a -> Maybe a
get x y m =
    let
        i =
            y * m.size + x
    in
        if x < 0 then
            Nothing
        else if y < 0 then
            Nothing
        else if x > (m.size - 1) then
            Nothing
        else
            Array.get i m.cells


map : (Array a -> a -> b) -> Matrix a -> Matrix b
map f m =
    let
        g i =
            let
                ( x, y ) =
                    ( modBy m.size i, i // m.size )
            in
                [ get (x - 1) (y - 1) m
                , get (x - 1) y m
                , get (x - 1) (y + 1) m
                , get x (y - 1) m
                , get x (y + 1) m
                , get (x + 1) (y - 1) m
                , get (x + 1) y m
                , get (x + 1) (y + 1) m
                ]
                    |> List.filterMap identity
                    |> Array.fromList
    in
        Matrix (m.cells |> Array.indexedMap (\i a -> f (g i) a)) m.size


next : Int -> Bool -> Bool
next a b =
    if b then
        (a == 2 || a == 3)
    else
        (a == 3)


type alias Model =
    Matrix Bool


type Msg
    = Tick Posix


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( Matrix
        (Array.fromList [ True, True, False, True, False, False, False, False, False, False, False, False, False, True, True, False, False, False, False, True, False, False, False, False, True, False, True, True, False, True, False, False, True, True, True, True, False, False, False, False, True, False, True, False, True, True, True, True, True, True, False, True, True, False, True, True, False, False, False, True, True, False, True, True, False, True, True, True, False, True, True, True, False, False, True, False, True, True, False, True, True, False, False, False, False, False, False, False, True, False, True, True, False, True, True, True, True, False, True, True, True, True, False, False, False, True, True, False, False, False, True, True, False, True, True, True, True, True, True, False, True, False, True, True, True, True, False, True, False, False, True, False, False, True, True, False, False, False, False, False, False, False, True, False, True, False, True, False, True, False, False, False, True, False, False, True, False, False, True, True, False, True, False, True, False, True, True, True, True, False, False, False, True, True, True, True, False, True, False, True, True, True, False, True, True, False, False, False, True, True, False, False, False, False, True, False, True, True, False, True, False, True, False, True, False, False, False, False, False, True, False, True, True, True, True, False, False, False, True, False, True, False, True, True, True, True, True, True, True, False, False, True, False, False, True, True, True, True, False, False, True, True, True, True, False, True, False, False, False, False, False, False, True, False, True, False, True, True, False, True, True, True, True, False, False, True, True, True, False, False, False, False, True, True, True, False, False, False, True, False, False, True, False, False, False, True, False, True, True, True, True, False, True, False, False, True, True, True, False, True, False, True, True, False, False, False, True, False, True, True, False, True, False, False, True, False, False, True, False, True, True, False, False, True, False, False, False, False, True, True, False, True, True, False, True, False, True, True, True, True, True, False, False, True, False, False, True, False, True, True, True, False, False, True, True, True, False, False, True, False, False, False, False, False, True, False, False, False, True, True, False, True, True, False, False, True, True, True, True, True, True, False, True, False, True, True, False, False, True, True, True, True, True, True, False, True, True, True, True, True, False, False, False, True, True, True, False, True, False, True, False, True, True, True, False, False, False, True, False, True, False, True, False, False, True, False, True, True, False, True, False, True, False, True, True, True, True, False, False, True, False, False, True, True, False, False, True, True, False, True, False, False, True, False, False, False, True, True, False, True, False, True, False, False, False, False, False, True, True, True, True, True, True, False, True, False, False, True, True, True, False, True, False, False, True, True, True, True, False, True, False, True, True, True, False, False, True, False, True, True, False, True, True, True, True, False, False, True, False, False, True, True, True, False, False, True, True, True, True, True, True, True, False, False, False, False, True, True, False, False, True, False, True, False, True, False, True, False, False, False, False, False, True, True, True, False, True, False, True, True, False, True, True, False, True, True, False, False, False, True, False, True, True, True, False, True, True, False, False, True, False, True, False, True, False, True, False, True, False, True, True, False, True, False, True, False, True, True, False, True, True, False, True, False, True, False, True, False, True, True, True, True, False, True, False, False, False, False, True, True, False, True, True, True, False, False, True, False, False, False, False, False, True, True, True, False, False, False, True, False, True, True, True, True, False, True, True, True, True, False, True, True, False, True, False, True, False, False, True, False, True, False, True, False, False, True, False, True, False, False, True, True, False, False, True, False, False, True, False, False, False, True, True, True, True, False, False, False, False, False, False, False, False, True, True, False, False, True, False, True, False, False, False, True, False, True, True, True, True, False, False, False, False, True, False, False, False, True, True, False, False, False, True, True, False, False, True, False, False, False, True, True, True, False, True, True, False, True, True, False, True, True, True, False, True, True, False, True, False, False, False, False, True, False, False, False, False, True, True, True, False, False, True, False, True, True, True, True, False, True, False, True, True, False, True, True, True, False, True, False, True, True, True, False, True, False, True, False, True, False, False, True, False, True, False, True, True, True, False, False, False, False, False, True, False, True, True, True, False, False, True, False, True, True, True, True, True, False, True, False, False, False, True, False, False, True, True, False, False, False, True, True, True, True, False, False, False, True, False, False, True, True, False, True, False, True, False, False, False, False, True, True, False, False, True, True, True, False, False, True, False, True, False, False, True, False, False, False, False, False, True, False, False, False, False, True, True, True, False, False, True, True, True, True, True, True, True, True, True, False, True, True, False, False, True, False, True, True, True, True, False, True, False, False, True, True, False, True, False, False, False, True, True, False, False, True, False, True, False, True, True, True, False, True, False, True, True, True, True, False, False, True, True, False, False, True, True, False, True, True, False, True, False, False, False, True, True, True, True, False, False, False, True, False, True, False, True, False, False, True, True, False, False, True, False, False, True, False, True, False, True, True, True, True, True, False, False, False, False, False, False, True, True, False, False, True, True, False, True, True, True, False, False, True, False, False, True, True, False, True, True, True, True, True, False, True, True, False, True, False, False, True, True, False, False, True, False, False, False, True, False, True, False, False, False, False, True, False, False, True, True, False, False, True ])
        32
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        unit =
            20

        size3 =
            model.size * unit

        fillColor a b =
            get a b model
                |> Maybe.map
                    (\c ->
                        if c then
                            "rgb(100, 100, 100)"
                        else
                            "rgb(255, 255, 255)"
                    )
                |> Maybe.withDefault "rgb(255, 255, 255)"
                |> fill
    in
        Html.div []
            [ svg
                [ width (String.fromInt size3)
                , height (String.fromInt size3)
                , viewBox ("0 0 " ++ String.fromInt size3 ++ " " ++ String.fromInt size3)
                ]
                (List.range 0 model.size |> List.map (\a -> List.range 0 model.size |> List.map (\b -> rect [ x (String.fromInt (a * unit)), y (String.fromInt (b * unit)), width (String.fromInt unit), height (String.fromInt unit), rx "10", ry "10", fillColor a b ] [])) |> List.concat)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        f a b =
            next (a |> Array.filter identity |> Array.length) b
    in
        case msg of
            Tick _ ->
                ( map f model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick
