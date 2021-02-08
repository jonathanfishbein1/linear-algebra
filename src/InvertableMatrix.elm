module InvertableMatrix exposing
    ( InvertableMatrix(..)
    , determinant
    , dimension
    , isInvertable
    , invert
    , multiply
    , multiplyMatrixVector
    , getAt
    , equal, projXOntoSubspace
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
@docs multiply
@docs multiplyMatrixVector


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
determinant vectorSpace (InvertableMatrix matrix) =
    SquareMatrix.upperTriangle vectorSpace matrix
        |> SquareMatrix.getDiagonalProduct vectorSpace.field
        |> Result.fromMaybe "Index out of range"


{-| Try to calculate the inverse of a matrix
-}
invert : Vector.InnerProductSpace a -> InvertableMatrix a -> Result String (InvertableMatrix a)
invert innerProductSpace (InvertableMatrix matrix) =
    case isInvertable innerProductSpace matrix of
        Ok invMatrix ->
            let
                sizeOfMatrix =
                    SquareMatrix.dimension invMatrix

                augmentedMatrix =
                    SquareMatrix.appendHorizontal invMatrix
                        (SquareMatrix.identity innerProductSpace.vectorSpace.field sizeOfMatrix)

                (SquareMatrix.SquareMatrix reducedRowEchelonForm) =
                    SquareMatrix.gaussJordan innerProductSpace.vectorSpace augmentedMatrix

                inverse =
                    SquareMatrix.subMatrix
                        0
                        (Matrix.mDimension reducedRowEchelonForm)
                        sizeOfMatrix
                        (Matrix.nDimension reducedRowEchelonForm)
                        (SquareMatrix.SquareMatrix reducedRowEchelonForm)
                        |> InvertableMatrix
            in
            Ok inverse

        Err err ->
            Err err


{-| Determine whether a matirx is invertable
-}
isInvertable : Vector.InnerProductSpace a -> SquareMatrix.SquareMatrix a -> Result String (SquareMatrix.SquareMatrix a)
isInvertable innerProductSpace (SquareMatrix.SquareMatrix matrix) =
    case Matrix.isOnto innerProductSpace matrix of
        Ok ontoMatrix ->
            case Matrix.isOneToOne innerProductSpace ontoMatrix of
                Ok ontoAndOneToOneMatrix ->
                    Ok (SquareMatrix.SquareMatrix ontoAndOneToOneMatrix)

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
    -> Matrix.ColumnVector a
    -> Result String (Matrix.ColumnVector a)
multiplyMatrixVector innerProductSpace (InvertableMatrix matrix) vector =
    SquareMatrix.multiplyMatrixVector innerProductSpace matrix vector


{-| Calculate the projection of a vector onto a subspace given by a list of basis vectors as column vectors
-}
projXOntoSubspace : Vector.InnerProductSpace a -> List (Matrix.ColumnVector a) -> Matrix.ColumnVector a -> Result String (Matrix.ColumnVector a)
projXOntoSubspace innerProductSpace columnVectorBasis x =
    let
        matrix =
            SquareMatrix.createMatrixFromColumnVectors columnVectorBasis

        transposeMatrix =
            SquareMatrix.transpose matrix

        transformationMatrix =
            SquareMatrix.multiply innerProductSpace transposeMatrix matrix
                |> Result.map InvertableMatrix
                |> Result.andThen (invert innerProductSpace)
                |> Result.andThen (\(InvertableMatrix invMatrix) -> SquareMatrix.multiply innerProductSpace matrix invMatrix)
                |> Result.andThen (\res -> SquareMatrix.multiply innerProductSpace res transposeMatrix)
    in
    Result.andThen (\tMatrix -> SquareMatrix.multiplyMatrixVector innerProductSpace tMatrix x) transformationMatrix


{-| Compare two matricies using comparator
-}
equal : (a -> a -> Bool) -> InvertableMatrix a -> InvertableMatrix a -> Bool
equal comparator (InvertableMatrix matrixOne) (InvertableMatrix matrixTwo) =
    SquareMatrix.equal comparator matrixOne matrixTwo
