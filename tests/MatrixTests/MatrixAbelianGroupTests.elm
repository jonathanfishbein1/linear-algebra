module MatrixTests.MatrixAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Matrix
import Parser
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Abelian Group Tests"
        [ Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests matrix inverse"
          <|
            \one two ->
                let
                    complexOneNegative =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real <|
                                Basics.negate
                                    1
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    v =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.one ]
                            ]

                    w =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ complexOneNegative ]
                            ]

                    zero =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.zero
                                    ]
                            ]
                in
                Matrix.add ComplexNumbers.complexField v w
                    |> Expect.equal zero
        ]
