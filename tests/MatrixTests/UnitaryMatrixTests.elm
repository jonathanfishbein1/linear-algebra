module MatrixTests.UnitaryMatrixTests exposing (suite)

import ComplexNumbers
import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import Imaginary
import InvertableMatrix
import Matrix
import NormalMatrix
import Real
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
                            (Real.Real
                                (1 / 2)
                            )
                            ((1 / 2)
                                |> Real.Real
                                |> Imaginary.Imaginary
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                0
                            )
                            ((1 / Basics.sqrt 3)
                                |> Real.Real
                                |> Imaginary.Imaginary
                            )

                    complexNumberR1C3 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (3 / (2 * Basics.sqrt 15))
                            )
                            ((1 / (2 * Basics.sqrt 15))
                                |> Real.Real
                                |> Imaginary.Imaginary
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (-1 / 2)
                            )
                            Imaginary.zero

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (1 / Basics.sqrt 3)
                            )
                            Imaginary.zero

                    complexNumberR2C3 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (4 / (2 * Basics.sqrt 15))
                            )
                            ((3 / (2 * Basics.sqrt 15))
                                |> Real.Real
                                |> Imaginary.Imaginary
                            )

                    complexNumberR3C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (1 / 2)
                            )
                            Imaginary.zero

                    complexNumberR3C2 =
                        ComplexNumbers.ComplexNumber
                            Real.zero
                            ((-1 / Basics.sqrt 3)
                                |> Real.Real
                                |> Imaginary.Imaginary
                            )

                    complexNumberR3C3 =
                        ComplexNumbers.ComplexNumber
                            Real.zero
                            ((5 / (2 * Basics.sqrt 15))
                                |> Real.Real
                                |> Imaginary.Imaginary
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
