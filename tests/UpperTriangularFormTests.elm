module UpperTriangularFormTests exposing (suite)

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
                            [ Matrix.RowVector <| Vector.Vector [ 3, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 6, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 4, 4 ]
                            ]

                    upperTriangularFormMatrix =
                        Matrix.upperTriangle matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 3.0, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 3.3333333333333335, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 4 ]
                            ]
                in
                Expect.equal upperTriangularFormMatrix (Ok expected)
        ]
