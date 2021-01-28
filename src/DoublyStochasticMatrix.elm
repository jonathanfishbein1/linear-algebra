module DoublyStochasticMatrix exposing
    ( DoublyStochasticMatrix(..)
    , isDoublyStochastic
    , multiply
    )

{-| A module for Doubly Stochastic Matrix


# Types

@docs DoublyStochasticMatrix


# Matrix Predicates and Properties

@docs isDoublyStochastic


# Binary Operations

@docs multiply

-}

import Matrix
import SquareMatrix
import Vector


{-| Doubly Stochastic Matrix type
-}
type DoublyStochasticMatrix
    = DoublyStochasticMatrix (SquareMatrix.SquareMatrix Float)


{-| Predicate if matrix is doubly stochastic
-}
isDoublyStochastic : SquareMatrix.SquareMatrix Float -> Bool
isDoublyStochastic (SquareMatrix.SquareMatrix matrix) =
    if SquareMatrix.isRightStochastic (SquareMatrix.SquareMatrix matrix) && SquareMatrix.isLeftStochastic (SquareMatrix.SquareMatrix matrix) then
        Matrix.all
            ((<=) 0)
            matrix

    else
        False


{-| Doubly Stochastic Matrixx Doubly Stochastic Matrix multiplication
-}
multiply :
    DoublyStochasticMatrix
    -> DoublyStochasticMatrix
    -> Result String DoublyStochasticMatrix
multiply (DoublyStochasticMatrix matrixOne) (DoublyStochasticMatrix matrixTwo) =
    SquareMatrix.multiply Vector.realInnerProductSpace matrixOne matrixTwo
        |> Result.map DoublyStochasticMatrix
