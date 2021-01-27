module InvertableMatrix exposing
    ( InvertableMatrix(..)
    , determinant
    , dimension
    , isInvertable
    , invert
    , getAt
    , multiply, multiplyMatrixVector
    )

{-| A module for Invertable Matrix


# Types

@docs InvertableMatrix


# Matrix Predicates and Properties

@docs determinant
@docs dimension
@docs isInvertable


# Binary Operations

@docs invert


# Manipulation

@docs getAt

-}

import Matrix
import SquareMatrix
import Vector


{-| Invertable Matrix type
-}
type InvertableMatrix a
    = InvertableMatrix (SquareMatrix.SquareMatrix a)


{-| Try to calculate the determinant
-}
determinant : Vector.VectorSpace a -> InvertableMatrix a -> Result String a
determinant vectorSpace (InvertableMatrix (SquareMatrix.SquareMatrix matrix)) =
    Matrix.upperTriangle vectorSpace matrix
        |> Matrix.getDiagonalProduct vectorSpace.field
        |> Result.fromMaybe "Index out of range"


{-| Try to calculate the inverse of a matrix
-}
invert : Vector.InnerProductSpace a -> InvertableMatrix a -> Result String (Matrix.Matrix a)
invert innerProductSpace (InvertableMatrix matrix) =
    case isInvertable innerProductSpace matrix of
        Ok invMatrix ->
            let
                sizeOfMatrix =
                    Matrix.mDimension invMatrix

                augmentedMatrix =
                    Matrix.appendHorizontal invMatrix
                        (Matrix.identity innerProductSpace.vectorSpace.field sizeOfMatrix)

                reducedRowEchelonForm =
                    Matrix.gaussJordan innerProductSpace.vectorSpace augmentedMatrix

                inverse =
                    Matrix.subMatrix
                        0
                        (Matrix.mDimension reducedRowEchelonForm)
                        sizeOfMatrix
                        (Matrix.nDimension reducedRowEchelonForm)
                        reducedRowEchelonForm
            in
            Ok inverse

        Err err ->
            Err err


{-| Determine whether a matirx is invertable
-}
isInvertable : Vector.InnerProductSpace a -> SquareMatrix.SquareMatrix a -> Result String (Matrix.Matrix a)
isInvertable innerProductSpace (SquareMatrix.SquareMatrix matrix) =
    case Matrix.isOnto innerProductSpace matrix of
        Ok ontoMatrix ->
            case Matrix.isOneToOne innerProductSpace ontoMatrix of
                Ok ontoAndOneToOneMatrix ->
                    Ok ontoAndOneToOneMatrix

                Err error ->
                    Err (error ++ " Matrix is not invertable")

        Err error ->
            Err (error ++ " Matrix is not invertable")


{-| Dimension of the matrix
-}
dimension : InvertableMatrix a -> Int
dimension (InvertableMatrix matrix) =
    SquareMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> InvertableMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (InvertableMatrix matrix) =
    SquareMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Invertable Matrix Invertable Matrix multiplication
-}
multiply :
    Vector.InnerProductSpace a
    -> InvertableMatrix a
    -> InvertableMatrix a
    -> Result String (InvertableMatrix a)
multiply innerProductSpace (InvertableMatrix matrixOne) (InvertableMatrix matrixTwo) =
    SquareMatrix.multiply innerProductSpace matrixOne matrixTwo
        |> Result.map InvertableMatrix


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> InvertableMatrix a
    -> Vector.Vector a
    -> Result String (Vector.Vector a)
multiplyMatrixVector innerProductSpace (InvertableMatrix matrix) vector =
    SquareMatrix.multiplyMatrixVector innerProductSpace matrix vector
