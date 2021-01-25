module HermitianMatrix exposing
    ( HermitianMatrix(..)
    , isHermitian
    , dimension
    , getAt
    )

{-| A module for Hermitian Matrix


# Types

@docs HermitianMatrix


# Matrix Predicates and Properties

@docs isHermitian
@docs dimension


# Manipulation

@docs getAt

-}

import ComplexNumbers
import Matrix
import SquareMatrix


{-| Hermitian Matrix type
-}
type HermitianMatrix a
    = HermitianMatrix (SquareMatrix.SquareMatrix a)


{-| Predicate to determine if Matrix is Hermitian
-}
isHermitian : SquareMatrix.SquareMatrix (ComplexNumbers.ComplexNumber number) -> Bool
isHermitian (SquareMatrix.SquareMatrix matrix) =
    Matrix.adjoint matrix == matrix


{-| Dimension of the matrix
-}
dimension : HermitianMatrix a -> Int
dimension (HermitianMatrix matrix) =
    SquareMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> HermitianMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (HermitianMatrix matrix) =
    SquareMatrix.getAt ( rowIndex, columnIndex ) matrix
