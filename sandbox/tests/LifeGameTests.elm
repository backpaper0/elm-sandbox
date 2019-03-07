module LifeGameTests exposing (..)

import Test exposing (..)
import Expect
import Array
import LifeGame exposing (..)


matrix1 =
    Matrix (Array.fromList [ 1, 2, 3, 4 ]) 2


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


suite : Test
suite =
    describe "LifeGame"
        [ describe "next"
            ([ ( False, 0, Expect.false )
             , ( False, 1, Expect.false )
             , ( False, 2, Expect.false )
             , ( False, 3, Expect.true )
             , ( False, 4, Expect.false )
             , ( False, 5, Expect.false )
             , ( False, 6, Expect.false )
             , ( False, 7, Expect.false )
             , ( False, 8, Expect.false )
             , ( True, 0, Expect.false )
             , ( True, 1, Expect.false )
             , ( True, 2, Expect.true )
             , ( True, 3, Expect.true )
             , ( True, 4, Expect.false )
             , ( True, 5, Expect.false )
             , ( True, 6, Expect.false )
             , ( True, 7, Expect.false )
             , ( True, 8, Expect.false )
             ]
                |> List.indexedMap (\i ( a, b, c ) -> test ("next " ++ (String.fromInt i)) (\_ -> c "" <| next b a))
            )
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
        , test "map matrix" <|
            (\_ -> Expect.equal matrix3 <| map (\a b -> (Array.foldl (+) 0 a) + b) matrix2)
        ]
