module MatrixTests.GaussTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
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
                            , Matrix.RowVector <| Vector.Vector [ 0, 0, 1, 3 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests matrix gaussianReduce put matrix into Row Echelon Form" <|
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
        , Test.test "tests gaussianReduceComplex put complex matrix into Row Echelon Form" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 6) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 3) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 4) (ComplexNumbers.Imaginary 0) ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduceComplex matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1.0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| 2.0 / 3) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| -1 / 3.3333333333333335) (ComplexNumbers.Imaginary 0) ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0), ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 0) ]
                            ]
                in
                Expect.true "matrics equal" (Matrix.equal ComplexNumbers.equal rowEchelonFormMatrix expected)
        ]
