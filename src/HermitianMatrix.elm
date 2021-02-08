module HermitianMatrix exposing
    ( HermitianMatrix(..)
    , isHermitian
    , dimension
    , identity
    , scalarMultiplication
    , multiply
    , multiplyMatrixVector
    , subtract
    , getAt
    )

{-| A module for Hermitian Matrix


# Types

@docs HermitianMatrix


# Matrix Predicates and Properties

@docs isHermitian
@docs dimension


# Unitary Operations

@docs identity
@docs scalarMultiplication


# Binary Operations

@docs multiply
@docs multiplyMatrixVector
@docs subtract


# Manipulation

@docs getAt

-}

import ComplexNumbers
import Matrix
import SquareMatrix
import SymmetricMatrix
import Vector


{-| Hermitian Matrix type
-}
type HermitianMatrix number
    = HermitianMatrix (SymmetricMatrix.SymmetricMatrix (ComplexNumbers.ComplexNumber number))


{-| Predicate to determine if Matrix is Hermitian
-}
isHermitian : SymmetricMatrix.SymmetricMatrix (ComplexNumbers.ComplexNumber number) -> Bool
isHermitian (SymmetricMatrix.SymmetricMatrix (SquareMatrix.SquareMatrix matrix)) =
    Matrix.adjoint matrix == matrix


{-| Dimension of the matrix
-}
dimension : HermitianMatrix number -> Int
dimension (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrix)) =
    SquareMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> HermitianMatrix number -> Maybe (ComplexNumbers.ComplexNumber number)
getAt ( rowIndex, columnIndex ) (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrix)) =
    SquareMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Hermitian Matrix Hermitian Matrix multiplication
-}
multiply :
    HermitianMatrix Float
    -> HermitianMatrix Float
    -> Result String (HermitianMatrix Float)
multiply (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrixOne)) (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrixTwo)) =
    SquareMatrix.multiply Vector.complexInnerProductSpace matrixOne matrixTwo
        |> Result.map SymmetricMatrix.SymmetricMatrix
        |> Result.map HermitianMatrix


{-| Multiply a Vector by a Hermitian Matrix
-}
multiplyMatrixVector :
    HermitianMatrix Float
    -> Matrix.ColumnVector (ComplexNumbers.ComplexNumber Float)
    -> Result String (Matrix.ColumnVector (ComplexNumbers.ComplexNumber Float))
multiplyMatrixVector (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrix)) vector =
    SquareMatrix.multiplyMatrixVector Vector.complexInnerProductSpace matrix vector


{-| Subtract two Hermitian Matrices
-}
subtract : HermitianMatrix Float -> HermitianMatrix Float -> HermitianMatrix Float
subtract (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrixOne)) (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrixTwo)) =
    SquareMatrix.subtract ComplexNumbers.complexField matrixOne matrixTwo
        |> SymmetricMatrix.SymmetricMatrix
        |> HermitianMatrix


{-| Create Square Identity Matrix with n dimension with Complex Numbers
-}
identity : Int -> HermitianMatrix Float
identity =
    SquareMatrix.identity ComplexNumbers.complexField
        >> SymmetricMatrix.SymmetricMatrix
        >> HermitianMatrix


{-| Scalar multiplication over a Hermitian Matrix
-}
scalarMultiplication : ComplexNumbers.ComplexNumber Float -> HermitianMatrix Float -> HermitianMatrix Float
scalarMultiplication scalar (HermitianMatrix (SymmetricMatrix.SymmetricMatrix matrix)) =
    SquareMatrix.scalarMultiplication ComplexNumbers.complexField scalar matrix
        |> SymmetricMatrix.SymmetricMatrix
        |> HermitianMatrix
