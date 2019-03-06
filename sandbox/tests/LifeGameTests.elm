module LifeGameTests exposing (..)

import Test exposing (..)
import Expect
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


cells =
    Array.fromList
        [ (Array.fromList [ 1, 2 ])
        , (Array.fromList [ 3, 4 ])
        ]


cells2 =
    Array.fromList
        [ (Array.fromList [ 1, 2, 4 ])
        , (Array.fromList [ 8, 16, 32 ])
        , (Array.fromList [ 64, 128, 256 ])
        ]


cells3 =
    Array.fromList
        [ (Array.fromList [ 1 + 2 + 8 + 16, 1 + 2 + 4 + 8 + 16 + 32, 2 + 4 + 16 + 32 ])
        , (Array.fromList [ 1 + 2 + 8 + 16 + 64 + 128, 1 + 2 + 4 + 8 + 16 + 32 + 64 + 128 + 256, 2 + 4 + 16 + 32 + 128 + 256 ])
        , (Array.fromList [ 8 + 16 + 64 + 128, 8 + 16 + 32 + 64 + 128 + 256, 16 + 32 + 128 + 256 ])
        ]


suite : Test
suite =
    describe "LifeGame"
        [ describe "next"
            [ test "birth" <|
                (\_ -> Expect.true "" <| next 3 False)
            , test "survival 2" <|
                (\_ -> Expect.true "" <| next 2 True)
            , test "survival 3" <|
                (\_ -> Expect.true "" <| next 3 True)
            , test "depopulation" <|
                (\_ -> Expect.false "" <| next 1 True)
            , test "crowded" <|
                (\_ -> Expect.false "" <| next 4 True)
            ]
        , describe "get"
            [ test "get 1" <|
                (\_ -> Expect.equal (Just 1) (get 0 0 cells))
            , test "get 2" <|
                (\_ -> Expect.equal (Just 2) (get 0 1 cells))
            , test "get 3" <|
                (\_ -> Expect.equal (Just 3) (get 1 0 cells))
            , test "get 4" <|
                (\_ -> Expect.equal (Just 4) (get 1 1 cells))
            , test "out of row range" <|
                (\_ -> Expect.equal Nothing (get 2 0 cells))
            , test "out of column range" <|
                (\_ -> Expect.equal Nothing (get 0 2 cells))
            ]
        , describe "map"
            [ test "map cells" <|
                (\_ -> Expect.equal cells3 <| map (\a b -> (List.sum (Array.toList a)) + b) cells2)
            ]
        ]
