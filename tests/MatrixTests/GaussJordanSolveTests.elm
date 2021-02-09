module MatrixTests.GaussJordanSolveTests exposing (suite)

import ColumnVector
import Expect
import Float.Extra
import Matrix
import RowVector
import Test
import Typeclasses.Classes.Equality
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

                    reducedRowEchelonFormMatrix =
                        Matrix.gaussJordan Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, -8.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, 1.0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -2.0 ]
                            ]
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

                    b =
                        ColumnVector.ColumnVector <| Vector.Vector [ -4, -11, 22 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve (Typeclasses.Classes.Equality.eq (Float.Extra.equalWithin 1.0e-6)) Vector.realInnerProductSpace matrix b

                    expected =
                        ColumnVector.ColumnVector <| Vector.Vector [ -8.0, 1.0, -2.0 ]
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

                    b =
                        ColumnVector.ColumnVector <| Vector.Vector [ 3, 0, -2 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve (Typeclasses.Classes.Equality.eq (Float.Extra.equalWithin 1.0e-6)) Vector.realInnerProductSpace matrix b

                    expected =
                        ColumnVector.ColumnVector <| Vector.Vector [ 5, -1.0, -1.0 ]
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

                    b =
                        ColumnVector.ColumnVector <| Vector.Vector [ 2, 1, 4 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve
                            (Typeclasses.Classes.Equality.eq (Float.Extra.equalWithin 1.0e-6))
                            Vector.realInnerProductSpace
                            matrix
                            b

                    expected =
                        ColumnVector.ColumnVector <| Vector.Vector [ 1.4285714285714286, 0.4285714285714286 ]
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

                    b =
                        ColumnVector.ColumnVector <| Vector.Vector [ 7, 12, 4 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve (Typeclasses.Classes.Equality.eq (Float.Extra.equalWithin 1.0e-6)) Vector.realInnerProductSpace matrix b
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

                    b =
                        ColumnVector.ColumnVector <| Vector.Vector [ 312, 184, 0, 0 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve (Typeclasses.Classes.Equality.eq (Float.Extra.equalWithin 1.0e-6)) Vector.realInnerProductSpace matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix
                    (Matrix.Consistant (Matrix.UniqueSolution (ColumnVector.ColumnVector (Vector.Vector [ 156, 312, 92, 184 ]))))
        ]
