module MatrixTests.MatrixAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Imaginary
import Matrix
import Real
import RowVector
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
                            (Real.Real <|
                                Basics.negate
                                    1
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    v =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.one ]
                            ]

                    w =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ complexOneNegative ]
                            ]

                    zero =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.zero
                                    ]
                            ]
                in
                Matrix.add ComplexNumbers.field v w
                    |> Expect.equal zero
        ]
