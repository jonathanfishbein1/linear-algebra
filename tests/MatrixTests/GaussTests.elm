module MatrixTests.GaussTests exposing (suite)

import ComplexNumbers
import Expect
import Float.Extra
import Imaginary
import Matrix
import Real
import RowVector
import Test
import Vector


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
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , RowVector.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce produces correct answers second example"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 1, 1, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 2, 3, 0 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 3, 4, -2 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 1.0, 1.0, 3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, 2.0, -3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce with infinite solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 1, 1, 7 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 2, 2, -1, 12 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 4, 0, 6, 4 ]
                            ]

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 2.0, 1.0, 1.0, 7 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -2.0, 5 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 0.0, 0.0, 0.0 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce with infinite solutions two"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 1, 3, 1, 4 ]
                            ]

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, 1, -2, 2 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce with no solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 2, 2, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 4, 0, 6 ]
                            ]

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, 1, -2 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, 0, 0 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduce wikibooks example"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 3, 9 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, -1, 1, 8 ]
                            , RowVector.RowVector <| Vector.Vector [ 3, 0, -1, 3 ]
                            ]

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 3, 9 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 1, 1, 2 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, 1, 3 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix
                in
                Expect.true "matrices are equal" ((Matrix.equal (Float.Extra.equalWithin 0.0001)).eq rowEchelonFormMatrix expected)
        , Test.test
            "tests matrix gaussianReduce put matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , RowVector.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test
            "tests gaussianReduceComplex put complex matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 2) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 6) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 1) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0) ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.complexVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 1.0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real <| 2.0 / 3) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 1) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 1) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real <| -1 / 3.3333333333333335) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 1) (Imaginary.Imaginary 0) ]
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
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                2
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                3
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                4
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce Vector.complexVectorSpace matrix

                    complexNumberExpectedR1C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    complexNumberExpectedR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                -1
                            )

                    complexNumberExpectedR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberExpectedR1C1, complexNumberExpectedR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.zero, complexNumberExpectedR2C2 ]
                            ]
                in
                Expect.true "matricies equal" ((Matrix.equal ComplexNumbers.equal.eq).eq rowEchelonFormMatrix expected)
        ]
