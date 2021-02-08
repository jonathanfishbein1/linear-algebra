module SymmetricMatrix exposing
    ( SymmetricMatrix(..)
    , isSymmetric
    , adjoint, dimension, getAt, identity, multiply, multiplyMatrixVector, scalarMultiplication, subtract
    )

{-| A module for Square Matrix


# Types

@docs SymmetricMatrix


# Matrix Predicates and Properties

@docs isSymmetric

-}

import ComplexNumbers
import Field
import Matrix
import NormalMatrix
import SquareMatrix
import Vector


{-| Symmetric Matrix type
-}
type SymmetricMatrix a
    = SymmetricMatrix (NormalMatrix.NormalMatrix a)


{-| Predicate to determine if Matrix is symmetric
-}
isSymmetric : NormalMatrix.NormalMatrix a -> Bool
isSymmetric (NormalMatrix.NormalMatrix matrix) =
    SquareMatrix.transpose matrix == matrix


{-| Perform the adjoint operation on a Complex Numbered Matrix
-}
adjoint :
    SymmetricMatrix (ComplexNumbers.ComplexNumber number)
    -> SymmetricMatrix (ComplexNumbers.ComplexNumber number)
adjoint (SymmetricMatrix matrix) =
    NormalMatrix.adjoint matrix
        |> SymmetricMatrix


{-| Dimension of the matrix
-}
dimension : SymmetricMatrix a -> Int
dimension (SymmetricMatrix matrix) =
    NormalMatrix.dimension matrix


{-| Square Matrix Square Matrix multiplication
-}
multiply :
    Vector.InnerProductSpace a
    -> SymmetricMatrix a
    -> SymmetricMatrix a
    -> Result String (SymmetricMatrix a)
multiply innerProductSpace (SymmetricMatrix matrixOne) (SymmetricMatrix matrixTwo) =
    NormalMatrix.multiply innerProductSpace matrixOne matrixTwo
        |> Result.map SymmetricMatrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> SymmetricMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (SymmetricMatrix matrix) =
    NormalMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> SymmetricMatrix a
    -> Matrix.ColumnVector a
    -> Result String (Matrix.ColumnVector a)
multiplyMatrixVector innerProductSpace (SymmetricMatrix matrix) vector =
    NormalMatrix.multiplyMatrixVector innerProductSpace matrix vector


{-| Subtract two Square Matrices
-}
subtract : Field.Field a -> SymmetricMatrix a -> SymmetricMatrix a -> SymmetricMatrix a
subtract field (SymmetricMatrix matrixOne) (SymmetricMatrix matrixTwo) =
    NormalMatrix.subtract field matrixOne matrixTwo
        |> SymmetricMatrix


{-| Create Square Identity Matrix with n dimension
-}
identity : Field.Field a -> Int -> SymmetricMatrix a
identity field =
    NormalMatrix.identity field
        >> SymmetricMatrix


{-| Scalar multiplication over a Square Matrix
-}
scalarMultiplication : Field.Field a -> a -> SymmetricMatrix a -> SymmetricMatrix a
scalarMultiplication field scalar (SymmetricMatrix matrix) =
    NormalMatrix.scalarMultiplication field scalar matrix
        |> SymmetricMatrix