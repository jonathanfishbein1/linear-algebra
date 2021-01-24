module HermitianMatrix exposing
    ( HermitianMatrix(..)
    , isHermitian
    )

{-| A module for Hermitian Matrix


# Types

@docs HermitianMatrix


# Matrix Predicates and Properties

@docs isHermitian

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
