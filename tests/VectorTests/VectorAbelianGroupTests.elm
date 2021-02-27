module VectorTests.VectorAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
import Real
import Test


suite : Test.Test
suite =
    Test.describe "Vector Abelian Group Tests"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
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
                        Internal.Vector.Vector
                            [ complexNumber ]

                    w =
                        Internal.Vector.Vector
                            [ complexNumber ]

                    zero =
                        Internal.Vector.Vector
                            [ ComplexNumbers.zero ]
                in
                Internal.Vector.subtract ComplexNumbers.field v w
                    |> Expect.equal zero
        ]
