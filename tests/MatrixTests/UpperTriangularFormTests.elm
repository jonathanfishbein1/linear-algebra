module MatrixTests.UpperTriangularFormTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
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
                            [ Matrix.RowVector <| Vector.Vector [ 3, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 6, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 4, 4 ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 3.0, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 3.3333333333333335, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 4 ]
                            ]
                in
                Expect.equal upperTriangularFormMatrix (Ok expected)
        , Test.test
            "tests gaussianReduce put complex matrix into Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 6) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0) ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.complexVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3.0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 2.0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3.3333333333333335) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real -1) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0) ]
                            ]
                in
                case upperTriangularFormMatrix of
                    Ok result ->
                        Expect.true "matrices are equal" (Matrix.equal ComplexNumbers.equal result expected)

                    Err error ->
                        Expect.fail error
        , Test.test
            "tests upperTriangleComplex puts matrix into upper tirangle form"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
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

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle Vector.complexVectorSpace matrix

                    complexNumberExpectedR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                3
                            )

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero, complexNumberExpectedR2C2 ]
                            ]
                in
                case upperTriangularFormMatrix of
                    Ok result ->
                        Expect.true "matrices are equal" (Matrix.equal ComplexNumbers.equal result expected)

                    Err error ->
                        Expect.fail error
        ]
