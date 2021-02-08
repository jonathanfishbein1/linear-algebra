module MatrixTests.UpperTriangularFormTests exposing (suite)

import ComplexNumbers
import Expect
import Matrix
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
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 6) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0) ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.complexVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real 3.0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 2.0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 3.3333333333333335) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real -1) (ComplexNumbers.Imaginary 0) ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumber (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0) ]
                            ]
                in
                Expect.true "matrices are equal" (Matrix.equal ComplexNumbers.equal upperTriangularFormMatrix expected)
        , Test.test
            "tests upperTriangleComplex puts matrix into upper tirangle form"
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
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.complexVectorSpace matrix

                    complexNumberExpectedR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                3
                            )

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.zero, complexNumberExpectedR2C2 ]
                            ]
                in
                Expect.true "matrices are equal" (Matrix.equal ComplexNumbers.equal upperTriangularFormMatrix expected)
        ]
