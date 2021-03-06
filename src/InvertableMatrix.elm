module InvertableMatrix exposing
    ( InvertableMatrix(..)
    , empty
    , identity
    , determinant
    , dimension
    , isInvertable
    , scalarMultiplication
    , invert
    , add
    , multiply
    , multiplyIfCan
    , multiplyMatrixVector
    , getAt
    , setAt
    , equal
    , equalImplementation
    , projXOntoSubspace
    )

{-| A module for Invertable Matrix


# Types

@docs InvertableMatrix


# Values

@docs empty


# Constructors

@docs identity


# Matrix Predicates and Properties

@docs determinant
@docs dimension
@docs isInvertable


# Unitary Operations

@docs scalarMultiplication


# Binary Operations

@docs invert
@docs add
@docs multiply
@docs multiplyIfCan
@docs multiplyMatrixVector


# Manipulation

@docs getAt
@docs setAt


# Equality

@docs equal
@docs equalImplementation

@docs projXOntoSubspace

-}

import ColumnVector
import Field
import Matrix
import NormalMatrix
import RowVector
import SquareMatrix
import Typeclasses.Classes.Equality


{-| Invertable Matrix type
-}
type InvertableMatrix a
    = InvertableMatrix (NormalMatrix.NormalMatrix a)


{-| Try to calculate the determinant
-}
determinant : RowVector.VectorSpace a -> InvertableMatrix a -> Result String a
determinant vectorSpace (InvertableMatrix matrix) =
    NormalMatrix.upperTriangle vectorSpace matrix
        |> NormalMatrix.getDiagonalProduct vectorSpace.field
        |> Result.fromMaybe "Index out of range"


{-| Try to calculate the inverse of a matrix
-}
invert : RowVector.InnerProductSpace a -> InvertableMatrix a -> Result String (InvertableMatrix a)
invert innerProductSpace (InvertableMatrix matrix) =
    case isInvertable innerProductSpace matrix of
        Ok invMatrix ->
            let
                sizeOfMatrix =
                    NormalMatrix.dimension invMatrix

                augmentedMatrix =
                    NormalMatrix.appendHorizontal invMatrix
                        (NormalMatrix.identity innerProductSpace.vectorSpace.field sizeOfMatrix)

                (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix reducedRowEchelonForm)) =
                    NormalMatrix.gaussJordan innerProductSpace.vectorSpace augmentedMatrix

                inverse =
                    SquareMatrix.subMatrix
                        0
                        (Matrix.mDimension reducedRowEchelonForm)
                        sizeOfMatrix
                        (Matrix.nDimension reducedRowEchelonForm)
                        (SquareMatrix.SquareMatrix reducedRowEchelonForm)
                        |> NormalMatrix.NormalMatrix
                        |> InvertableMatrix
            in
            Ok inverse

        Err err ->
            Err err


{-| Determine whether a matirx is invertable
-}
isInvertable : RowVector.InnerProductSpace a -> NormalMatrix.NormalMatrix a -> Result String (NormalMatrix.NormalMatrix a)
isInvertable innerProductSpace (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix matrix)) =
    case Matrix.isOnto innerProductSpace matrix of
        Ok ontoMatrix ->
            case Matrix.isOneToOne innerProductSpace ontoMatrix of
                Ok ontoAndOneToOneMatrix ->
                    Ok (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix ontoAndOneToOneMatrix))

                Err error ->
                    Err (error ++ " Matrix is not invertable")

        Err error ->
            Err (error ++ " Matrix is not invertable")


{-| Dimension of the matrix
-}
dimension : InvertableMatrix a -> Int
dimension (InvertableMatrix matrix) =
    NormalMatrix.dimension matrix


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> InvertableMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (InvertableMatrix matrix) =
    NormalMatrix.getAt ( rowIndex, columnIndex ) matrix


{-| Set the value in a Normal Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> a -> InvertableMatrix a -> InvertableMatrix a
setAt tup element (InvertableMatrix matrix) =
    NormalMatrix.setAt tup element matrix
        |> InvertableMatrix


{-| Add two InvertableMatrix together
-}
add : Field.Field a -> InvertableMatrix a -> InvertableMatrix a -> InvertableMatrix a
add field (InvertableMatrix matrixOne) (InvertableMatrix matrixTwo) =
    NormalMatrix.add field matrixOne matrixTwo
        |> InvertableMatrix


{-| Square Matrix Square Matrix multiplication
-}
multiply :
    RowVector.InnerProductSpace a
    -> InvertableMatrix a
    -> InvertableMatrix a
    -> InvertableMatrix a
multiply innerProductSpace (InvertableMatrix matrixOne) (InvertableMatrix matrixTwo) =
    NormalMatrix.multiply innerProductSpace matrixOne matrixTwo
        |> InvertableMatrix


{-| Invertable Matrix Invertable Matrix multiplication
-}
multiplyIfCan :
    RowVector.InnerProductSpace a
    -> InvertableMatrix a
    -> InvertableMatrix a
    -> Result String (InvertableMatrix a)
multiplyIfCan innerProductSpace (InvertableMatrix matrixOne) (InvertableMatrix matrixTwo) =
    NormalMatrix.multiplyIfCan innerProductSpace matrixOne matrixTwo
        |> Result.map InvertableMatrix


{-| Multiply a ColumnVector by a Matrix
-}
multiplyMatrixVector :
    RowVector.InnerProductSpace a
    -> InvertableMatrix a
    -> ColumnVector.ColumnVector a
    -> Result String (ColumnVector.ColumnVector a)
multiplyMatrixVector innerProductSpace (InvertableMatrix matrix) vector =
    NormalMatrix.multiplyMatrixVector innerProductSpace matrix vector


{-| Calculate the projection of a vector onto a subspace given by a list of basis vectors as column vectors
-}
projXOntoSubspace : RowVector.InnerProductSpace a -> List (ColumnVector.ColumnVector a) -> ColumnVector.ColumnVector a -> Result String (ColumnVector.ColumnVector a)
projXOntoSubspace innerProductSpace columnVectorBasis x =
    let
        matrix =
            NormalMatrix.createMatrixFromColumnVectors columnVectorBasis

        transposeMatrix =
            NormalMatrix.transpose matrix

        transformationMatrix =
            NormalMatrix.multiplyIfCan innerProductSpace transposeMatrix matrix
                |> Result.map InvertableMatrix
                |> Result.andThen (invert innerProductSpace)
                |> Result.andThen (\(InvertableMatrix invMatrix) -> NormalMatrix.multiplyIfCan innerProductSpace matrix invMatrix)
                |> Result.andThen (\res -> NormalMatrix.multiplyIfCan innerProductSpace res transposeMatrix)
    in
    Result.andThen (\tMatrix -> NormalMatrix.multiplyMatrixVector innerProductSpace tMatrix x) transformationMatrix


{-| Compare two Matrices for equality
-}
equalImplementation : (a -> a -> Bool) -> InvertableMatrix a -> InvertableMatrix a -> Bool
equalImplementation comparator (InvertableMatrix matrixOne) (InvertableMatrix matrixTwo) =
    NormalMatrix.equalImplementation comparator matrixOne matrixTwo


{-| Compare two matricies using comparator
-}
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (InvertableMatrix a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Create Square Identity Matrix with n dimension
-}
identity : Field.Field a -> Int -> InvertableMatrix a
identity field =
    NormalMatrix.identity field
        >> InvertableMatrix


{-| Scalar multiplication over an InvertableMatrix Matrix
-}
scalarMultiplication : Field.Field a -> a -> InvertableMatrix a -> InvertableMatrix a
scalarMultiplication field scalar (InvertableMatrix matrix) =
    NormalMatrix.scalarMultiplication field scalar matrix
        |> InvertableMatrix


{-| Monoid empty for InvertableMatrix
-}
empty : InvertableMatrix a
empty =
    NormalMatrix.empty
        |> InvertableMatrix
