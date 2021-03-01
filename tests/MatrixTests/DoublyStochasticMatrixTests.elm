module MatrixTests.DoublyStochasticMatrixTests exposing (suite)

import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import Internal.Vector
import Matrix
import Real
import RowVector
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
                            [ RowVector.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    isDoublyStochastic =
                        DoublyStochasticMatrix.isDoublyStochastic matrix
                in
                Expect.equal (Ok matrix) isDoublyStochastic
        ]
