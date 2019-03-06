module LifeGameTests exposing (..)

import Test exposing (..)
import Expect
import Array
import LifeGame exposing (..)


matrix1 =
    Matrix (Array.fromList [ 1, 2, 3, 4 ]) 2 2


matrix2 =
    Matrix
        ([ [ 1, 2, 4 ]
         , [ 8, 16, 32 ]
         , [ 64, 128, 256 ]
         ]
            |> List.concat
            |> Array.fromList
        )
        3
        3


matrix3 =
    Matrix
        ([ [ 1 + 2 + 8 + 16, 1 + 2 + 4 + 8 + 16 + 32, 2 + 4 + 16 + 32 ]
         , [ 1 + 2 + 8 + 16 + 64 + 128, 1 + 2 + 4 + 8 + 16 + 32 + 64 + 128 + 256, 2 + 4 + 16 + 32 + 128 + 256 ]
         , [ 8 + 16 + 64 + 128, 8 + 16 + 32 + 64 + 128 + 256, 16 + 32 + 128 + 256 ]
         ]
            |> List.concat
            |> Array.fromList
        )
        3
        3


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
                (\_ -> Expect.equal (Just 1) (get 0 0 matrix1))
            , test "get 2" <|
                (\_ -> Expect.equal (Just 2) (get 1 0 matrix1))
            , test "get 3" <|
                (\_ -> Expect.equal (Just 3) (get 0 1 matrix1))
            , test "get 4" <|
                (\_ -> Expect.equal (Just 4) (get 1 1 matrix1))
            , test "out of row range minus" <|
                (\_ -> Expect.equal Nothing (get 1 -1 matrix1))
            , test "out of row range" <|
                (\_ -> Expect.equal Nothing (get 0 2 matrix1))
            , test "out of column range minus" <|
                (\_ -> Expect.equal Nothing (get -1 1 matrix1))
            , test "out of column range" <|
                (\_ -> Expect.equal Nothing (get 2 0 matrix1))
            ]
        , describe "map"
            [ test "map matrix" <|
                (\_ -> Expect.equal matrix3 <| map (\a b -> (Array.foldl (+) 0 a) + b) matrix2)
            ]
        ]
