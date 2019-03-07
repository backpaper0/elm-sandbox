module LifeGame exposing (..)

import Array exposing (Array)
import Browser
import Svg
import Svg.Attributes as SvgAttrs
import Time exposing (Posix)
import Html exposing (..)
import Random exposing (Generator)


type alias Matrix a =
    { cells : Array a, size : Int }


index2pos : Int -> Int -> ( Int, Int )
index2pos size i =
    let
        x =
            modBy size i

        y =
            i // size
    in
        ( x, y )


pos2index : Int -> ( Int, Int ) -> Int
pos2index size ( x, y ) =
    y * size + x


get : Int -> Int -> Matrix a -> Maybe a
get x y m =
    let
        i =
            pos2index m.size ( x, y )
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
                    index2pos m.size i
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
    { matrix : Matrix Bool
    , unit : Int
    }


type Msg
    = Tick Posix
    | Init (Array Bool)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    let
        size =
            32

        gen1 =
            Random.int 0 1 |> Random.map ((==) 0)

        gen2 =
            Random.list (size * size) gen1

        gen3 =
            Random.map Array.fromList gen2
    in
        ( { matrix = Matrix Array.empty size, unit = 20 }, Random.generate Init gen3 )


view : Model -> Html Msg
view { matrix, unit } =
    let
        sizeText =
            matrix.size * unit |> String.fromInt

        fillColor x y =
            let
                bool2maybe b =
                    if b then
                        Just "#4dbfd2"
                    else
                        Nothing
            in
                get x y matrix
                    |> Maybe.andThen bool2maybe
                    |> Maybe.withDefault "rgb(255, 255, 255)"
                    |> SvgAttrs.fill

        makeCell i =
            let
                ( x, y ) =
                    index2pos matrix.size i
            in
                Svg.rect
                    [ SvgAttrs.x (String.fromInt (x * unit))
                    , SvgAttrs.y (String.fromInt (y * unit))
                    , SvgAttrs.width (String.fromInt unit)
                    , SvgAttrs.height (String.fromInt unit)
                    , SvgAttrs.rx "8"
                    , SvgAttrs.ry "8"
                    , fillColor x y
                    ]
                    []
    in
        Html.div []
            [ Svg.svg
                [ SvgAttrs.width sizeText
                , SvgAttrs.height sizeText
                , SvgAttrs.viewBox ("0 0 " ++ sizeText ++ " " ++ sizeText)
                ]
                (List.range 0 (matrix.size * matrix.size) |> List.map makeCell)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        f a b =
            next (a |> Array.filter identity |> Array.length) b
    in
        case msg of
            Init cells ->
                let
                    m1 =
                        model.matrix

                    m2 =
                        { m1 | cells = cells }
                in
                    ( { model | matrix = m2 }, Cmd.none )

            Tick _ ->
                if (Array.isEmpty model.matrix.cells) then
                    ( model, Cmd.none )
                else
                    ( { model | matrix = map f model.matrix }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick
