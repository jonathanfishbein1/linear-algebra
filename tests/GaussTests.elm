module GaussTests exposing (suite)

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
        [ Test.test "tests gaussianReduce put matrix into Row Echelon Form" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , Matrix.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests gaussianReduce produces correct answers second example" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 1, 1, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 2, 3, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 3, 4, -2 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 1.0, 1.0, 3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, 2.0, -3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -1.0 ]
                            ]
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests gaussianReduce with infinite solutions" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 1, 1, 7 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 2, 2, -1, 12 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 4, 0, 6, 4 ]
                            ]

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 2.0, 1.0, 1.0, 7 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -2.0, 5 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 0.0, 0.0, 0.0 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests gaussianReduce with infinite solutions two" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 3, 1, 4 ]
                            ]

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 0, 1, -2, 2 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests gaussianReduce with no solutions" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 2, 2, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 4, 0, 6 ]
                            ]

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 0, 1, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 0, 0, 0 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests gaussianReduce wikibooks example" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 3, 9 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, -1, 1, 8 ]
                            , Matrix.RowVector <| Vector.Vector [ 3, 0, -1, 3 ]
                            ]

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 3, 9 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 1, 1, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 0, 1, 2.9999999999999996 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        ]
