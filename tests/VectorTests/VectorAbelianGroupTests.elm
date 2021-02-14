module VectorTests.VectorAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Vector Abelian Group Tests"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests vector inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                one
                            )
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
                Vector.subtract ComplexNumbers.complexField v w
                    |> Expect.equal zero
        ]
