module MatrixTests.UpperTriangularFormTests exposing (suite)

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
    Test.describe "The LinearAlgebra module"
        [ Test.test
            "tests gaussianReduce put matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 3, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 4, 6, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 4, 4 ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 3.0, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 3.3333333333333335, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 4 ]
                            ]
                in
                Expect.equal upperTriangularFormMatrix expected
        , Test.test
            "tests upperTriangular github"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 0, 1, 0, 0, 312 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, 0, 1, 184 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, -2, 1, 0 ]
                            , RowVector.RowVector <| Vector.Vector [ -2, 1, 0, 0, 0 ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ -2, 1, 0, 0, 0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 1, 0, 0, 312 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, -2, 1, 0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 0, 0, 1, 184 ]
                            ]
                in
                Expect.equal upperTriangularFormMatrix expected
        , Test.test
            "tests gaussianReduce put complex matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 2) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 6) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 1) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0) ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.complexVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 3.0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 2.0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 3) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 3.3333333333333335) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real -1) (Imaginary.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (Real.Real 0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 0) (Imaginary.Imaginary 0), ComplexNumbers.ComplexNumber (Real.Real 4) (Imaginary.Imaginary 0) ]
                            ]
                in
                Expect.true "matrices are equal" ((Matrix.equal ComplexNumbers.equal.eq).eq upperTriangularFormMatrix expected)
        , Test.test
            "tests upperTriangleComplex puts matrix into upper tirangle form"
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

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.complexVectorSpace matrix

                    complexNumberExpectedR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                3
                            )

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.zero, complexNumberExpectedR2C2 ]
                            ]
                in
                Expect.true "matrices are equal" ((Matrix.equal ComplexNumbers.equal.eq).eq upperTriangularFormMatrix expected)
        ]
