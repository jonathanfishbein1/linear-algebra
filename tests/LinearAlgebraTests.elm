module LinearAlgebraTests exposing (suite)

import Expect
import Fuzz
import LinearAlgebra
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests ComplexNumbers add" <|
            \one two ->
                let
                    testValue =
                        LinearAlgebra.Vector [ one, two ]

                    expected =
                        LinearAlgebra.Vector [ 2 * one, 2 * two ]
                in
                LinearAlgebra.add (+) testValue testValue
                    |> Expect.equal expected
        ]
