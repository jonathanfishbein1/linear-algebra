module UnitaryMatrix exposing
    ( UnitaryMatrix(..)
    , isUnitary
    , dimension
    , getAt
    )

{-| A module for Unitary Matrix


# Types

@docs UnitaryMatrix


# Matrix Predicates and Properties

@docs isUnitary
@docs dimension


# Manipulation

@docs getAt

-}

import ComplexNumbers
import InvertableMatrix
import Matrix
import SquareMatrix
import Vector


{-| Unitary Matrix type
-}
type UnitaryMatrix a
    = UnitaryMatrix (InvertableMatrix.InvertableMatrix a)


{-| Determine whether a matirx is unitary
-}
isUnitary : InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber Float) -> Bool
isUnitary (InvertableMatrix.InvertableMatrix (SquareMatrix.SquareMatrix matrix)) =
    case InvertableMatrix.invert Vector.complexInnerProductSpace (SquareMatrix.SquareMatrix matrix) of
        Ok inverse ->
            Matrix.equal ComplexNumbers.equal inverse (Matrix.adjoint matrix)

        Err _ ->
            False


{-| Dimension of the matrix
-}
dimension : UnitaryMatrix a -> Int
dimension (UnitaryMatrix matrix) =
    InvertableMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> UnitaryMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (UnitaryMatrix matrix) =
    InvertableMatrix.getAt ( rowIndex, columnIndex ) matrix
