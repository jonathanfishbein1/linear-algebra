module LinearAlgebraTests exposing (suite)

import Expect
import Fuzz
import LinearAlgebra
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumbers add" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| 2 * real) (ComplexNumbers.Imaginary <| 2 * imaginary)
                in
                ComplexNumbers.add testValue testValue
                    |> Expect.equal expected
        ]
