module VectorTests.VectorAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Vector Abelian Group Tests"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests vector inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    v =
                        Vector.Vector
                            [ complexNumber ]

                    w =
                        Vector.Vector
                            [ complexNumber ]

                    zero =
                        Vector.Vector
                            [ ComplexNumbers.zero ]
                in
                Internal.Vector.subtract ComplexNumbers.field v w
                    |> Expect.equal zero
        ]
