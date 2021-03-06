module HermitianMatrix exposing
    ( HermitianMatrix(..)
    , isHermitian
    , dimension
    , identity
    , scalarMultiplication
    , multiplyIfCan
    , multiplyMatrixVector
    , subtract
    , getAt
    , setAt
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

@docs multiplyIfCan
@docs multiplyMatrixVector
@docs subtract


# Manipulation

@docs getAt
@docs setAt

-}

import ColumnVector
import ComplexNumbers
import RowVector
import SymmetricMatrix


{-| Hermitian Matrix type
-}
type HermitianMatrix number
    = HermitianMatrix (SymmetricMatrix.SymmetricMatrix (ComplexNumbers.ComplexNumber number))


{-| Predicate to determine if Matrix is Hermitian
-}
isHermitian : SymmetricMatrix.SymmetricMatrix (ComplexNumbers.ComplexNumber number) -> Result String (SymmetricMatrix.SymmetricMatrix (ComplexNumbers.ComplexNumber number))
isHermitian matrix =
    if SymmetricMatrix.adjoint matrix == matrix then
        Ok matrix

    else
        Err "Adjoint of A not equal to A"


{-| Dimension of the matrix
-}
dimension : HermitianMatrix number -> Int
dimension (HermitianMatrix matrix) =
    SymmetricMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> HermitianMatrix number -> Maybe (ComplexNumbers.ComplexNumber number)
getAt ( rowIndex, columnIndex ) (HermitianMatrix matrix) =
    SymmetricMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Set the value in a Hermitian Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> ComplexNumbers.ComplexNumber number -> HermitianMatrix number -> HermitianMatrix number
setAt tup element (HermitianMatrix matrix) =
    SymmetricMatrix.setAt tup element matrix
        |> HermitianMatrix


{-| Hermitian Matrix Hermitian Matrix multiplication
-}
multiplyIfCan :
    HermitianMatrix Float
    -> HermitianMatrix Float
    -> Result String (HermitianMatrix Float)
multiplyIfCan (HermitianMatrix matrixOne) (HermitianMatrix matrixTwo) =
    SymmetricMatrix.multiplyIfCan RowVector.complexInnerProductSpace matrixOne matrixTwo
        |> Result.map HermitianMatrix


{-| Multiply a ColumnVector by a Hermitian Matrix
-}
multiplyMatrixVector :
    HermitianMatrix Float
    -> ColumnVector.ColumnVector (ComplexNumbers.ComplexNumber Float)
    -> Result String (ColumnVector.ColumnVector (ComplexNumbers.ComplexNumber Float))
multiplyMatrixVector (HermitianMatrix matrix) vector =
    SymmetricMatrix.multiplyMatrixVector RowVector.complexInnerProductSpace matrix vector


{-| Subtract two Hermitian Matrices
-}
subtract : HermitianMatrix Float -> HermitianMatrix Float -> HermitianMatrix Float
subtract (HermitianMatrix matrixOne) (HermitianMatrix matrixTwo) =
    SymmetricMatrix.subtract ComplexNumbers.field matrixOne matrixTwo
        |> HermitianMatrix


{-| Create Square Identity Matrix with n dimension with Complex Numbers
-}
identity : Int -> HermitianMatrix Float
identity =
    SymmetricMatrix.identity ComplexNumbers.field
        >> HermitianMatrix


{-| Scalar multiplication over a Hermitian Matrix
-}
scalarMultiplication : ComplexNumbers.ComplexNumber Float -> HermitianMatrix Float -> HermitianMatrix Float
scalarMultiplication scalar (HermitianMatrix matrix) =
    SymmetricMatrix.scalarMultiplication ComplexNumbers.field scalar matrix
        |> HermitianMatrix
