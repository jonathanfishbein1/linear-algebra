module MatrixTests.InvertableMatrixTests exposing (suite)

import ComplexNumbers
import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import Field
import Imaginary
import InvertableMatrix
import Matrix
import NormalMatrix
import Real
import RowVector
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
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2 ]
                            , RowVector.RowVector <| Vector.Vector [ 3, 4 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinant =
                        InvertableMatrix.determinant RowVector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok (Real.Real -2))
        , Test.test
            "tests matrix determinant 3 x 3"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 4 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, -1, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 4, 0, 1 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinant =
                        InvertableMatrix.determinant RowVector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok (Real.Real 35))
        , Test.test
            "tests matrix determinant 4 x 4"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 3, 4 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 0, 2, 0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 3, 0, 0 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinant =
                        InvertableMatrix.determinant RowVector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok (Real.Real 7))
        , Test.test
            "tests matrix invert"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    expectedInverse =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , RowVector.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    inverse =
                        InvertableMatrix.invert RowVector.realInnerProductSpace matrix
                in
                Expect.equal inverse (Ok expectedInverse)
        , Test.test
            "tests matrix times inverse equals identity"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]
                            |> Matrix.map Real.Real

                    inverse =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , RowVector.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]
                            |> Matrix.map Real.Real

                    identity =
                        Matrix.identity Field.float (Matrix.mDimension matrix)
                            |> Matrix.map Real.Real

                    matrixInverseProduct =
                        Matrix.multiply RowVector.realInnerProductSpace matrix inverse
                in
                Expect.equal matrixInverseProduct (Ok identity)
        , Test.test
            "tests inverse times matrix equals identity"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]
                            |> Matrix.map Real.Real

                    inverse =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , RowVector.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]
                            |> Matrix.map Real.Real

                    identity =
                        Matrix.identity Field.float (Matrix.mDimension matrix)
                            |> Matrix.map Real.Real

                    inverseMatrixProduct =
                        Matrix.multiply RowVector.realInnerProductSpace inverse matrix
                in
                Expect.equal inverseMatrixProduct (Ok identity)
        , Test.test
            "tests complex matrix determinant 2 x 2"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            (Imaginary.Imaginary
                                Real.one
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                2
                            )
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                3
                            )
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                4
                            )
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    determinantComplex =
                        InvertableMatrix.determinant RowVector.complexVectorSpace matrix

                    expectedDeterminant =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                -2
                            )
                            (Imaginary.Imaginary
                                (Real.Real 4)
                            )
                in
                case determinantComplex of
                    Ok dComplex ->
                        Expect.true "determinants are equal" (ComplexNumbers.equal.eq dComplex expectedDeterminant)

                    _ ->
                        Expect.fail "determinants not equal"
        , Test.test
            "tests complex matrix inverse 2 x 2"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            (Imaginary.Imaginary
                                Real.one
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                2
                            )
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                3
                            )
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                4
                            )
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix

                    inverseComplex =
                        InvertableMatrix.invert RowVector.complexInnerProductSpace matrix

                    expectedComplexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                -(2 / 5)
                            )
                            (Imaginary.Imaginary
                                (Real.Real -(4 / 5))
                            )

                    expectedComplexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (1 / 5)
                            )
                            (Imaginary.Imaginary
                                (Real.Real (2 / 5))
                            )

                    expectedComplexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (3 / 10)
                            )
                            (Imaginary.Imaginary
                                (Real.Real (3 / 5))
                            )

                    expectedComplexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                (1 / 10)
                            )
                            (Imaginary.Imaginary
                                (Real.Real -(3 / 10))
                            )

                    expectedInverse =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ expectedComplexNumberR1C1, expectedComplexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ expectedComplexNumberR2C1, expectedComplexNumberR2C2 ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> InvertableMatrix.InvertableMatrix
                in
                case inverseComplex of
                    Ok result ->
                        Expect.true "matrices are equal" ((InvertableMatrix.equal ComplexNumbers.equal.eq).eq result expectedInverse)

                    Err error ->
                        Expect.fail error
        , Test.test
            "tests invertability"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 2, 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 3 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix

                    isInvertable =
                        InvertableMatrix.isInvertable RowVector.realInnerProductSpace matrix
                in
                Expect.equal isInvertable (Err "Matrix not onto Matrix is not invertable")
        , Test.test
            "tests invertabilityComplex"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix

                    isInvertable =
                        InvertableMatrix.isInvertable RowVector.complexInnerProductSpace matrix
                in
                Expect.equal isInvertable (Err "Matrix not onto Matrix is not invertable")
        ]
