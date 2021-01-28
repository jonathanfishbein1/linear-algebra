module UnitaryMatrix exposing
    ( UnitaryMatrix(..)
    , isUnitary
    , dimension
    , multiply
    , multiplyMatrixVector
    , getAt
    )

{-| A module for Unitary Matrix


# Types

@docs UnitaryMatrix


# Matrix Predicates and Properties

@docs isUnitary
@docs dimension


# Binary Operations

@docs multiply
@docs multiplyMatrixVector


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
type UnitaryMatrix number
    = UnitaryMatrix (InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber number))


{-| Determine whether a matirx is unitary
-}
isUnitary : InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber Float) -> Bool
isUnitary (InvertableMatrix.InvertableMatrix (SquareMatrix.SquareMatrix matrix)) =
    case InvertableMatrix.invert Vector.complexInnerProductSpace (InvertableMatrix.InvertableMatrix (SquareMatrix.SquareMatrix matrix)) of
        Ok inverse ->
            Matrix.equal ComplexNumbers.equal inverse (Matrix.adjoint matrix)

        Err _ ->
            False


{-| Dimension of the matrix
-}
dimension : UnitaryMatrix number -> Int
dimension (UnitaryMatrix matrix) =
    InvertableMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> UnitaryMatrix number -> Maybe (ComplexNumbers.ComplexNumber number)
getAt ( rowIndex, columnIndex ) (UnitaryMatrix matrix) =
    InvertableMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Unitary Matrix Unitary Matrix multiplication
-}
multiply :
    UnitaryMatrix Float
    -> UnitaryMatrix Float
    -> Result String (UnitaryMatrix Float)
multiply (UnitaryMatrix matrixOne) (UnitaryMatrix matrixTwo) =
    InvertableMatrix.multiply Vector.complexInnerProductSpace matrixOne matrixTwo
        |> Result.map UnitaryMatrix


{-| Multiply a Vector by a Unitary Matrix
-}
multiplyMatrixVector :
    UnitaryMatrix Float
    -> Vector.Vector (ComplexNumbers.ComplexNumber Float)
    -> Result String (Vector.Vector (ComplexNumbers.ComplexNumber Float))
multiplyMatrixVector (UnitaryMatrix matrix) vector =
    InvertableMatrix.multiplyMatrixVector Vector.complexInnerProductSpace matrix vector
