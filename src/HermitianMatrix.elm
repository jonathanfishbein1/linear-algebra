module HermitianMatrix exposing
    ( HermitianMatrix(..)
    , isHermitian
    , dimension
    , multiply
    , getAt
    , multiplyMatrixVector
    )

{-| A module for Hermitian Matrix


# Types

@docs HermitianMatrix


# Matrix Predicates and Properties

@docs isHermitian
@docs dimension


# Binary Operations

@docs multiply


# Manipulation

@docs getAt

-}

import ComplexNumbers
import Matrix
import SquareMatrix
import Vector


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


{-| Hermitian Matrix Hermitian Matrix multiplication
-}
multiply :
    Vector.InnerProductSpace a
    -> HermitianMatrix a
    -> HermitianMatrix a
    -> Result String (HermitianMatrix a)
multiply innerProductSpace (HermitianMatrix matrixOne) (HermitianMatrix matrixTwo) =
    SquareMatrix.multiply innerProductSpace matrixOne matrixTwo
        |> Result.map HermitianMatrix


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> HermitianMatrix a
    -> Vector.Vector a
    -> Result String (Vector.Vector a)
multiplyMatrixVector innerProductSpace (HermitianMatrix matrix) vector =
    SquareMatrix.multiplyMatrixVector innerProductSpace matrix vector
