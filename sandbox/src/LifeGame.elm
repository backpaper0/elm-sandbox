module LifeGame exposing (..)

import Array exposing (Array)
import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


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
        (a > 2)


main =
    let
        size1 =
            32

        size2 =
            20

        size3 =
            size * size2
    in
    svg [ width (String.fromInt size3), height (String.fromInt size3), viewBox ("0 0 " ++ String.fromInt size3 ++ " " ++ String.fromInt size3) ] (List.range 0 size1 |> List.map (\a -> List.range 0 size1 |> List.map (\b -> rect [ x (String.fromInt (a * size2)), y (String.fromInt (b * size2)), width (String.fromInt size2), height (String.fromInt size2), rx "10", ry "10" ] [])) |> List.concat)
