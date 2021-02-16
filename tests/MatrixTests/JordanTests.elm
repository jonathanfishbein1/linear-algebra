module MatrixTests.JordanTests exposing (suite)

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
            "tests jordanReduce put matrix into Reduced Row Echelon Form"
          <|
            \_ ->
                let
                    rowEchelonFormMatrix =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.jordanReduce Vector.realVectorSpace rowEchelonFormMatrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, -8.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, 1.0 ]
                            , RowVector.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test
            "tests jordanReduce produces correct answers second example"
          <|
            \_ ->
                let
                    rowEchelonFormMatrix =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 1.0, 1.0, 3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, 2.0, -3.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.jordanReduce Vector.realVectorSpace rowEchelonFormMatrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, 5.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, -1.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test
            "tests jordanReduceComplex put complex matrix into Reduced Row Echelon Form complex entries with imaginary portion"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                -1
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                0
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , RowVector.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.jordanReduce Vector.complexVectorSpace matrix

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ]
                            , RowVector.RowVector <| Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.one ]
                            ]
                in
                Expect.true "matricies equal" ((Matrix.equal ComplexNumbers.equal.eq).eq reducedRowEchelonFormMatrix expected)
        ]
