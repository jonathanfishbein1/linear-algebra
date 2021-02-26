module NormalMatrix exposing
    ( NormalMatrix(..)
    , createMatrixFromColumnVectors
    , identity
    , dimension
    , isNormalMatrix
    , getDiagonalProduct
    , subMatrix
    , transpose
    , scalarMultiplication
    , adjoint
    , add
    , multiply
    , multiplyMatrixVector
    , subtract
    , getAt
    , setAt
    , appendHorizontal
    , equal
    , equalImplementation
    , gaussJordan
    , upperTriangle
    )

{-| A module for Normal Matrix


# Types

@docs NormalMatrix


# Constructors

@docs createMatrixFromColumnVectors
@docs identity


# Matrix Predicates and Properties

@docs dimension
@docs isNormalMatrix
@docs getDiagonalProduct
@docs subMatrix
@docs transpose


# Unitary Operations

@docs scalarMultiplication
@docs adjoint


# Binary Operations

@docs add
@docs multiply
@docs multiplyMatrixVector
@docs subtract


# Manipulation

@docs getAt
@docs setAt


# Monoid

@docs appendHorizontal


# Equality

@docs equal
@docs equalImplementation


# Matrix Forms

@docs gaussJordan
@docs upperTriangle

-}

import ColumnVector
import ComplexNumbers
import Field
import Matrix
import SquareMatrix
import Typeclasses.Classes.Equality
import Vector


{-| Symmetric Matrix type
-}
type NormalMatrix a
    = NormalMatrix (SquareMatrix.SquareMatrix a)


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


{-| Add two NormalMatrix together
-}
add : Field.Field a -> NormalMatrix a -> NormalMatrix a -> NormalMatrix a
add field (NormalMatrix matrixOne) (NormalMatrix matrixTwo) =
    SquareMatrix.add field matrixOne matrixTwo
        |> NormalMatrix


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


{-| Set the value in a Normal Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> a -> NormalMatrix a -> NormalMatrix a
setAt tup element (NormalMatrix matrix) =
    SquareMatrix.setAt tup element matrix
        |> NormalMatrix


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> NormalMatrix a
    -> ColumnVector.ColumnVector a
    -> Result String (ColumnVector.ColumnVector a)
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


{-| Predicate to determine if Matrix is normal
-}
isNormalMatrix : Vector.InnerProductSpace a -> SquareMatrix.SquareMatrix a -> Bool
isNormalMatrix innerProductSpace (SquareMatrix.SquareMatrix matrix) =
    Matrix.multiply innerProductSpace (Matrix.transpose matrix) matrix == Matrix.multiply innerProductSpace matrix (Matrix.transpose matrix)


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : Vector.VectorSpace a -> NormalMatrix a -> NormalMatrix a
upperTriangle vectorSpace (NormalMatrix matrix) =
    SquareMatrix.upperTriangle vectorSpace matrix
        |> NormalMatrix


{-| Get the Product of the diagonal of a Matrix
-}
getDiagonalProduct : Field.Field a -> NormalMatrix a -> Maybe a
getDiagonalProduct field (NormalMatrix matrix) =
    SquareMatrix.getDiagonalProduct field matrix


{-| Create a Matrix from a list of Column Vectors
-}
createMatrixFromColumnVectors : List (ColumnVector.ColumnVector a) -> NormalMatrix a
createMatrixFromColumnVectors =
    SquareMatrix.createMatrixFromColumnVectors
        >> NormalMatrix


{-| Transpose a Matrix
-}
transpose : NormalMatrix a -> NormalMatrix a
transpose (NormalMatrix matrix) =
    SquareMatrix.transpose matrix
        |> NormalMatrix


{-| Append Matricies together horizontally
-}
appendHorizontal : NormalMatrix a -> NormalMatrix a -> NormalMatrix a
appendHorizontal (NormalMatrix matrixOne) (NormalMatrix matrixTwo) =
    SquareMatrix.appendHorizontal matrixOne matrixTwo
        |> NormalMatrix


{-| Function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : Vector.VectorSpace a -> NormalMatrix a -> NormalMatrix a
gaussJordan vectorSpace (NormalMatrix matrix) =
    SquareMatrix.gaussJordan vectorSpace matrix
        |> NormalMatrix


{-| Calculate the submatrix given a starting and ending row and column index
-}
subMatrix : Int -> Int -> Int -> Int -> NormalMatrix a -> NormalMatrix a
subMatrix startingRowIndex endingRowIndex startingColumnIndex endingColumnIndex (NormalMatrix matrix) =
    SquareMatrix.subMatrix startingRowIndex endingRowIndex startingColumnIndex endingColumnIndex matrix
        |> NormalMatrix


{-| Compare two Matrices for equality
-}
equalImplementation : (a -> a -> Bool) -> NormalMatrix a -> NormalMatrix a -> Bool
equalImplementation comparator (NormalMatrix matrixOne) (NormalMatrix matrixTwo) =
    SquareMatrix.equalImplementation comparator matrixOne matrixTwo


{-| Compare two matricies using comparator
-}
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (NormalMatrix a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)
