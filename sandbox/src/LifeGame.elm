module LifeGame exposing (..)

import Array


get r c cs =
    Array.get r cs |> Maybe.andThen (Array.get c)


map f cs =
    let
        g i j =
            [ get (i - 1) (j - 1) cs
            , get (i - 1) j cs
            , get (i - 1) (j + 1) cs
            , get i (j - 1) cs
            , get i (j + 1) cs
            , get (i + 1) (j - 1) cs
            , get (i + 1) j cs
            , get (i + 1) (j + 1) cs
            ]
                |> List.filterMap identity
                |> Array.fromList
    in
        cs |> Array.indexedMap (\i -> Array.indexedMap (\j -> g i j |> f))


next x a =
    if a then
        (x == 2 || x == 3)
    else
        (x > 2)
