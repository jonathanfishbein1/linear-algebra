module MatrixTests.GaussJordanSolveTests exposing (suite)

import ColumnVector
import Expect
import Matrix
import Real
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.test
            "tests matrix jordanReduce put matrix into Reduced Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , RowVector.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]
                            |> Matrix.map Real.Real

                    reducedRowEchelonFormMatrix =
                        Matrix.gaussJordan Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, -8.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, 1.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -2.0 ]
                            ]
                            |> Matrix.map Real.Real
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test
            "tests matrix gaussJordan produces correct answers"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 3, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ -2, 0, -3 ]
                            ]
                            |> Matrix.map Real.Real

                    b =
                        Vector.Vector [ -4, -11, 22 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Real.equal Vector.realInnerProductSpace matrix b

                    expected =
                        Vector.Vector [ -8.0, 1.0, -2.0 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.Consistant (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix gaussJordan produces correct answers second example"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 1, 1 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 3, 4 ]
                            ]
                            |> Matrix.map Real.Real

                    b =
                        Vector.Vector [ 3, 0, -2 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Real.equal Vector.realInnerProductSpace matrix b

                    expected =
                        Vector.Vector [ 5, -1.0, -1.0 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.Consistant (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix gaussJordan with no solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 2, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 2 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 1 ]
                            ]
                            |> Matrix.map Real.Real

                    b =
                        Vector.Vector [ 2, 1, 4 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real

                    reducedRowEchelonFormMatrix =
                        Matrix.solve
                            Real.equal
                            Vector.realInnerProductSpace
                            matrix
                            b

                    expected =
                        Vector.Vector [ 1.4285714285714286, 0.4285714285714286 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.Inconsistant "No Unique Solution: Least Squares Solution Provided" (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix gaussJordan with infinite solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 2, 2, -1 ]
                            , RowVector.RowVector <| Vector.Vector [ 2, 4, 0, 6 ]
                            ]
                            |> Matrix.map Real.Real

                    b =
                        Vector.Vector [ 7, 12, 4 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Real.equal Vector.realInnerProductSpace matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix
                    (Matrix.Consistant (Matrix.InfiniteSolutions { nullity = 3, rank = 2 }))
        , Test.test
            "tests matrix solve, github issue 2"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0 ])
                            , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1 ])
                            , RowVector.RowVector (Vector.Vector [ 0, 0, -2, 1 ])
                            , RowVector.RowVector (Vector.Vector [ -2, 1, 0, 0 ])
                            ]
                            |> Matrix.map Real.Real

                    b =
                        Vector.Vector [ 312, 184, 0, 0 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Real.equal Vector.realInnerProductSpace matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix
                    (Matrix.Consistant (Matrix.UniqueSolution (ColumnVector.ColumnVector (Vector.Vector [ Real.Real 156, Real.Real 312, Real.Real 92, Real.Real 184 ]))))
        ]
