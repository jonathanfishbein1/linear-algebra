module MatrixTests.MatrixAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Abelian Group Tests"
        [ Test.test
            "tests matrix inverse"
          <|
            \_ ->
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
