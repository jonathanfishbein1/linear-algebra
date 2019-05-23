module JordanTests exposing (suite)

import ComplexNumbers
import Expect
import Float.Extra
import Fuzz
import List.Extra
import Matrix
import Monoid
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
        ]
