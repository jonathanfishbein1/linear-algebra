module DoublyStochasticMatrix exposing
    ( DoublyStochasticMatrix(..)
    , isDoublyStochastic
    )

import Matrix
import SquareMatrix


{-| Doubly Stochastic Matrix type
-}
type DoublyStochasticMatrix a
    = DoublyStochasticMatrix (SquareMatrix.SquareMatrix a)


{-| Predicate if matrix is doubly stochastic
-}
isDoublyStochastic : SquareMatrix.SquareMatrix Float -> Bool
isDoublyStochastic (SquareMatrix.SquareMatrix matrix) =
    if SquareMatrix.isRightStochastic matrix && SquareMatrix.isLeftStochastic matrix then
        Matrix.all
            ((<=) 0)
            matrix

    else
        False
