module MatrixTests.GaussTests exposing (suite)

import ComplexNumbers
import Expect
import Imaginary
import Internal.Vector
import Matrix
import Real
import RowVector
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.test
            "tests gaussianReduce put matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, -1, -4 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 2, 3, -1, -11 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ -2, 0, -3, 22 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                            |> Matrix.map Real.Real
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce produces correct answers second example"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 1, 1, 3 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 3, 0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 1, 3, 4, -2 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1.0, 1.0, 1.0, 3.0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0.0, 1.0, 2.0, -3.0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]
                            |> Matrix.map Real.Real
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce with infinite solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 1, 1, 7 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 2, -1, 12 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 2, 4, 0, 6, 4 ]
                            ]
                            |> Matrix.map Real.Real

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1.0, 2.0, 1.0, 1.0, 7 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0.0, 0.0, 1.0, -2.0, 5 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0.0, 0.0, 0.0, 0.0, 0.0 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce with infinite solutions two"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 1, 1, 3, 1, 4 ]
                            ]
                            |> Matrix.map Real.Real

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0, 0, 1, -2, 2 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce with no solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 1, 1 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 2, -1 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 2, 4, 0, 6 ]
                            ]
                            |> Matrix.map Real.Real

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 1, 1 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0, 0, 1, -2 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0, 0, 0, 0 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce wikibooks example"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 3, 9 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 2, -1, 1, 8 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 3, 0, -1, 3 ]
                            ]
                            |> Matrix.map Real.Real

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, 3, 9 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0, 1, 1, 2 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0, 0, 1, 3 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix
                in
                Expect.true "matrices are equal" ((Matrix.equal Real.equal.eq).eq rowEchelonFormMatrix expected)
        , Test.test
            "tests matrix gaussianReduce put matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1, 2, -1, -4 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 2, 3, -1, -11 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ -2, 0, -3, 22 ]
                            ]
                            |> Matrix.map Real.Real

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                            |> Matrix.map Real.Real
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduceComplex put complex matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real 2) (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary Real.zero) ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real 6) (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary Real.zero) ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.ComplexNumber Real.one (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary Real.zero) ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.complexVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.ComplexNumber Real.one (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real <| 2.0 / 3) (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber Real.one (Imaginary.Imaginary Real.zero) ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.ComplexNumber Real.zero (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber Real.one (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber (Real.Real <| -1 / 3.3333333333333335) (Imaginary.Imaginary Real.zero) ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.ComplexNumber Real.zero (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber Real.zero (Imaginary.Imaginary Real.zero), ComplexNumbers.ComplexNumber Real.one (Imaginary.Imaginary Real.zero) ]
                            ]
                in
                Expect.true "matrics equal" ((Matrix.equal ComplexNumbers.equal.eq).eq rowEchelonFormMatrix expected)
        , Test.test
            "tests gaussianReduceComplex put complex matrix into Row Echelon Form complex entries with imaginary portion"
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
                            Imaginary.zero

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
                            [ RowVector.RowVector <| Internal.Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce RowVector.complexVectorSpace matrix

                    complexNumberExpectedR1C1 =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    complexNumberExpectedR1C2 =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            (Imaginary.Imaginary
                                (Real.Real
                                    -1
                                )
                            )

                    complexNumberExpectedR2C2 =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            (Imaginary.Imaginary
                                Real.zero
                            )

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Internal.Vector.Vector [ complexNumberExpectedR1C1, complexNumberExpectedR1C2 ]
                            , RowVector.RowVector <| Internal.Vector.Vector [ ComplexNumbers.zero, complexNumberExpectedR2C2 ]
                            ]
                in
                Expect.true "matricies equal" ((Matrix.equal ComplexNumbers.equal.eq).eq rowEchelonFormMatrix expected)
        ]
