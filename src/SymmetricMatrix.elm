module SymmetricMatrix exposing
    ( SymmetricMatrix(..)
    , isSymmetric
    )

{-| A module for Square Matrix


# Types

@docs SymmetricMatrix


# Matrix Predicates and Properties

@docs isSymmetric

-}

import Matrix
import SquareMatrix


{-| Symmetric Matrix type
-}
type SymmetricMatrix a
    = SymmetricMatrix (SquareMatrix.SquareMatrix a)


{-| Predicate to determine if Matrix is symmetric
-}
isSymmetric : SquareMatrix.SquareMatrix a -> Bool
isSymmetric (SquareMatrix.SquareMatrix matrix) =
    Matrix.transpose matrix == matrix
