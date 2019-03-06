module LifeGame exposing (..)

import Array exposing (Array)


type alias Matrix a =
    { cells : Array a, width : Int, height : Int }


get : Int -> Int -> Matrix a -> Maybe a
get x y m =
    let
        i =
            y * m.width + x
    in
        if x < 0 then
            Nothing
        else if y < 0 then
            Nothing
        else if x > (m.width - 1) then
            Nothing
        else
            Array.get i m.cells


map : (Array a -> a -> b) -> Matrix a -> Matrix b
map f m =
    let
        g i =
            let
                ( x, y ) =
                    ( modBy m.width i, i // m.width )
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
        Matrix (m.cells |> Array.indexedMap (\i a -> f (g i) a)) m.width m.height


next x a =
    if a then
        (x == 2 || x == 3)
    else
        (x > 2)
