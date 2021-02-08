module MatrixTests.UnitaryMatrixTests exposing (suite)

import ComplexNumbers
import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import InvertableMatrix
import Matrix
import NormalMatrix
import RowVector
import SquareMatrix
import Test
import UnitaryMatrix
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test
            "tests complex matrix inverse 3 x 3 is unitary"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                (1 / 2)
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (1 / Basics.sqrt 3)
                            )

                    complexNumberR1C3 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (3 / (2 * Basics.sqrt 15))
                            )
                            (ComplexNumbers.Imaginary
                                (1 / (2 * Basics.sqrt 15))
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (-1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / Basics.sqrt 3)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C3 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (4 / (2 * Basics.sqrt 15))
                            )
                            (ComplexNumbers.Imaginary
                                (3 / (2 * Basics.sqrt 15))
                            )

                    complexNumberR3C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR3C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (-1 / Basics.sqrt 3)
                            )

                    complexNumberR3C3 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (5 / (2 * Basics.sqrt 15))
                            )

                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2, complexNumberR1C3 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2, complexNumberR2C3 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR3C1, complexNumberR3C2, complexNumberR3C3 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    isUnitary =
                        UnitaryMatrix.isUnitary matrix
                in
                Expect.true "is unitary" isUnitary
        ]
