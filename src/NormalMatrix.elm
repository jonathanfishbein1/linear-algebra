module NormalMatrix exposing
    ( NormalMatrix(..)
    , adjoint
    , dimension
    , getAt
    , identity
    , isNormal
    , multiply
    , multiplyMatrixVector
    , scalarMultiplication
    , subtract
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
import SquareMatrix
import Vector


{-| Symmetric Matrix type
-}
type NormalMatrix a
    = NormalMatrix (SquareMatrix.SquareMatrix a)



-- {-| Predicate to determine if Matrix is symmetric
-- -}
-- isSymmetric : SquareMatrix.SquareMatrix a -> Bool
-- isSymmetric (SquareMatrix.SquareMatrix matrix) =
--     Matrix.transpose matrix == matrix


{-| Perform the adjoint operation on a Complex Numbered Matrix
-}
adjoint :
    NormalMatrix (ComplexNumbers.ComplexNumber number)
    -> NormalMatrix (ComplexNumbers.ComplexNumber number)
adjoint (NormalMatrix matrix) =
    SquareMatrix.adjoint matrix
        |> NormalMatrix


{-| Dimension of the matrix
-}
dimension : NormalMatrix a -> Int
dimension (NormalMatrix matrix) =
    SquareMatrix.dimension matrix


{-| Square Matrix Square Matrix multiplication
-}
multiply :
    Vector.InnerProductSpace a
    -> NormalMatrix a
    -> NormalMatrix a
    -> Result String (NormalMatrix a)
multiply innerProductSpace (NormalMatrix matrixOne) (NormalMatrix matrixTwo) =
    SquareMatrix.multiply innerProductSpace matrixOne matrixTwo
        |> Result.map NormalMatrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> NormalMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (NormalMatrix matrix) =
    SquareMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> NormalMatrix a
    -> Matrix.ColumnVector a
    -> Result String (Matrix.ColumnVector a)
multiplyMatrixVector innerProductSpace (NormalMatrix matrix) vector =
    SquareMatrix.multiplyMatrixVector innerProductSpace matrix vector


{-| Subtract two Square Matrices
-}
subtract : Field.Field a -> NormalMatrix a -> NormalMatrix a -> NormalMatrix a
subtract field (NormalMatrix matrixOne) (NormalMatrix matrixTwo) =
    SquareMatrix.subtract field matrixOne matrixTwo
        |> NormalMatrix


{-| Create Square Identity Matrix with n dimension
-}
identity : Field.Field a -> Int -> NormalMatrix a
identity field =
    SquareMatrix.identity field
        >> NormalMatrix


{-| Scalar multiplication over a Square Matrix
-}
scalarMultiplication : Field.Field a -> a -> NormalMatrix a -> NormalMatrix a
scalarMultiplication field scalar (NormalMatrix matrix) =
    SquareMatrix.scalarMultiplication field scalar matrix
        |> NormalMatrix


{-| Predicate to determine if Matrix is symmetric
-}
isNormal : Vector.InnerProductSpace a -> SquareMatrix.SquareMatrix a -> Bool
isNormal innerProductSpace (SquareMatrix.SquareMatrix matrix) =
    Matrix.multiply innerProductSpace (Matrix.transpose matrix) matrix == Matrix.multiply innerProductSpace matrix (Matrix.transpose matrix)
