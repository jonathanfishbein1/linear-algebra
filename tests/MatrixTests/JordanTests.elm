module MatrixTests.JordanTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.test "tests jordanReduce put matrix into Reduced Row Echelon Form" <|
            \_ ->
                let
                    rowEchelonFormMatrix =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.jordanReduce rowEchelonFormMatrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, -8.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, 1.0 ]
                            , Matrix.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test "tests jordanReduce produces correct answers second example" <|
            \_ ->
                let
                    rowEchelonFormMatrix =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 1.0, 1.0, 3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, 2.0, -3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.jordanReduce rowEchelonFormMatrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, 5.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, -1.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test "tests jordanReduceComplex put complex matrix into Reduced Row Echelon Form complex entries with imaginary portion" <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                -1
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.jordanReduceComplex matrix

                    complexOne =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexOne, ComplexNumbers.zero ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero, complexOne ]
                            ]
                in
                Expect.equal (Debug.log "reducedRowEchelonFormMatrix " reducedRowEchelonFormMatrix) (Debug.log "expected " expected)
        ]
