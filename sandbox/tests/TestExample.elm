module TestExample exposing (..)

import Test exposing (..)
import Expect


suite : Test
suite =
    test "test" <|
        (\_ -> Expect.true "test" True)
