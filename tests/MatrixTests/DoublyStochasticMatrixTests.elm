module MatrixTests.DoublyStochasticMatrixTests exposing (suite)

import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import Matrix
import SquareMatrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test
            "tests doubly stochastic"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> SquareMatrix.SquareMatrix

                    isDoublyStochastic =
                        DoublyStochasticMatrix.isDoublyStochastic matrix
                in
                Expect.true "Is Doubly Stochastic" isDoublyStochastic
        ]
