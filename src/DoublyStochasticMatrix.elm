module DoublyStochasticMatrix exposing
    ( DoublyStochasticMatrix(..)
    , isDoublyStochastic
    , multiply
    , setAt
    )

{-| A module for Doubly Stochastic Matrix


# Types

@docs DoublyStochasticMatrix


# Matrix Predicates and Properties

@docs isDoublyStochastic


# Binary Operations

@docs multiply


# Manipulation

@docs setAt

-}

import Real
import RowVector
import SquareMatrix


{-| Doubly Stochastic Matrix type
-}
type DoublyStochasticMatrix
    = DoublyStochasticMatrix (SquareMatrix.SquareMatrix (Real.Real Float))


{-| Predicate if matrix is doubly stochastic
-}
isDoublyStochastic : SquareMatrix.SquareMatrix (Real.Real Float) -> Result String (SquareMatrix.SquareMatrix (Real.Real Float))
isDoublyStochastic matrix =
    let
        right =
            SquareMatrix.isRightStochastic matrix

        left =
            SquareMatrix.isLeftStochastic matrix
    in
    Result.map2
        (\_ _ ->
            matrix
        )
        right
        left
        |> Result.andThen
            (\_ ->
                if
                    SquareMatrix.all
                        (Real.greaterThan Real.zero)
                        matrix
                then
                    Ok matrix

                else
                    Err "Matrix cannot have negative values"
            )



-- case SquareMatrix.isRightStochastic (SquareMatrix.SquareMatrix matrix)
-- if SquareMatrix.isRightStochastic (SquareMatrix.SquareMatrix matrix) && SquareMatrix.isLeftStochastic (SquareMatrix.SquareMatrix matrix) then
--     Matrix.all
--         (Real.greaterThan Real.zero)
--         matrix
-- else
--     False


{-| Doubly Stochastic Matrixx Doubly Stochastic Matrix multiplication
-}
multiply :
    DoublyStochasticMatrix
    -> DoublyStochasticMatrix
    -> Result String DoublyStochasticMatrix
multiply (DoublyStochasticMatrix matrixOne) (DoublyStochasticMatrix matrixTwo) =
    SquareMatrix.multiply RowVector.realInnerProductSpace matrixOne matrixTwo
        |> Result.map DoublyStochasticMatrix


{-| Set the value in a Doubly Stochastic Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> Real.Real Float -> DoublyStochasticMatrix -> DoublyStochasticMatrix
setAt tup element (DoublyStochasticMatrix matrix) =
    SquareMatrix.setAt tup element matrix
        |> DoublyStochasticMatrix
