module VectorMonadTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Monad abstraction for Vector"
        [ Test.fuzz Fuzz.int "tests Vector Monad left identity" <|
            \one ->
                let
                    f a =
                        [ a * 2 ]
                            |> Vector.Vector

                    leftSide =
                        Vector.bind (Vector.pure one) f

                    rightSide =
                        f one
                in
                Expect.equal leftSide rightSide
        ]
