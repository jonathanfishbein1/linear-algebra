module MatrixTests.InvertableMatrixTests exposing (suite)

import ComplexNumbers
import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import Field
import InvertableMatrix
import Matrix
import NormalMatrix
import SquareMatrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test
            "tests matrix determinant 2 x 2"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 3, 4 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinant =
                        InvertableMatrix.determinant Vector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok -2)
        , Test.test
            "tests matrix determinant 3 x 3"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, -1, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 0, 1 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinant =
                        InvertableMatrix.determinant Vector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok 35)
        , Test.test
            "tests matrix determinant 4 x 4"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 3, 4 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 0, 2, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, 0, 0 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinant =
                        InvertableMatrix.determinant Vector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok 7)
        , Test.test
            "tests matrix invert"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    expectedInverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    inverse =
                        InvertableMatrix.invert Vector.realInnerProductSpace matrix
                in
                Expect.equal inverse (Ok expectedInverse)
        , Test.test
            "tests matrix times inverse equals identity"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]

                    inverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]

                    identity =
                        Matrix.identity Field.float (Matrix.mDimension matrix)

                    matrixInverseProduct =
                        Matrix.multiply Vector.realInnerProductSpace matrix inverse
                in
                Expect.equal matrixInverseProduct (Ok identity)
        , Test.test
            "tests inverse times matrix equals identity"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]

                    inverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]

                    identity =
                        Matrix.identity Field.float (Matrix.mDimension matrix)

                    inverseMatrixProduct =
                        Matrix.multiply Vector.realInnerProductSpace inverse matrix
                in
                Expect.equal inverseMatrixProduct (Ok identity)
        , Test.test
            "tests complex matrix determinant 2 x 2"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                4
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinantComplex =
                        InvertableMatrix.determinant Vector.complexVectorSpace matrix

                    expectedDeterminant =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                -2
                            )
                            (ComplexNumbers.Imaginary
                                4
                            )
                in
                case determinantComplex of
                    Ok dComplex ->
                        Expect.true "determinants are equal" (ComplexNumbers.equal dComplex expectedDeterminant)

                    _ ->
                        Expect.fail "determinants not equal"
        , Test.test
            "tests complex matrix inverse 2 x 2"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                4
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    inverseComplex =
                        InvertableMatrix.invert Vector.complexInnerProductSpace matrix

                    expectedComplexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                -(2 / 5)
                            )
                            (ComplexNumbers.Imaginary
                                -(4 / 5)
                            )

                    expectedComplexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 5)
                            )
                            (ComplexNumbers.Imaginary
                                (2 / 5)
                            )

                    expectedComplexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (3 / 10)
                            )
                            (ComplexNumbers.Imaginary
                                (3 / 5)
                            )

                    expectedComplexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 10)
                            )
                            (ComplexNumbers.Imaginary
                                -(3 / 10)
                            )

                    expectedInverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ expectedComplexNumberR1C1, expectedComplexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ expectedComplexNumberR2C1, expectedComplexNumberR2C2 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix
                in
                case inverseComplex of
                    Ok result ->
                        Expect.true "matrices are equal" (InvertableMatrix.equal ComplexNumbers.equal result expectedInverse)

                    Err error ->
                        Expect.fail error
        , Test.test
            "tests invertability"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 2, 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 3 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix

                    isInvertable =
                        InvertableMatrix.isInvertable Vector.realInnerProductSpace matrix
                in
                Expect.equal isInvertable (Err "Matrix not onto Matrix is not invertable")
        , Test.test
            "tests invertabilityComplex"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix

                    isInvertable =
                        InvertableMatrix.isInvertable Vector.complexInnerProductSpace matrix
                in
                Expect.equal isInvertable (Err "Matrix not onto Matrix is not invertable")
        ]
