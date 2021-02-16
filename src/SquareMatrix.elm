module SquareMatrix exposing
    ( SquareMatrix(..)
    , InnerProductSpace
    , zeroSquareMatrix
    , realMatrixInnerProductSpace
    , complexMatrixInnerProductSpace
    , createMatrixFromColumnVectors
    , identity
    , dimension
    , isSquareMatrix
    , normReal
    , normComplex
    , distanceReal
    , isRightStochastic
    , isLeftStochastic
    , getDiagonalProduct
    , subMatrix
    , transpose
    , scalarMultiplication
    , adjoint
    , map
    , dotProduct
    , multiply
    , multiplyMatrixVector
    , add
    , subtract
    , getAt
    , appendHorizontal
    , equal
    , equalImplementation
    , gaussJordan
    , upperTriangle
    )

{-| A module for Square Matrix


# Types

@docs SquareMatrix
@docs InnerProductSpace


# Values

@docs zeroSquareMatrix
@docs realMatrixInnerProductSpace
@docs complexMatrixInnerProductSpace


# Constructors

@docs createMatrixFromColumnVectors
@docs identity


# Matrix Predicates and Properties

@docs dimension
@docs isSquareMatrix
@docs normReal
@docs normComplex
@docs distanceReal
@docs isRightStochastic
@docs isLeftStochastic
@docs getDiagonalProduct
@docs subMatrix
@docs transpose


# Unitary Operations

@docs scalarMultiplication
@docs adjoint
@docs map


# Binary Operations

@docs dotProduct
@docs multiply
@docs multiplyMatrixVector
@docs add
@docs subtract


# Manipulation

@docs getAt


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
import Real
import RowVector
import Typeclasses.Classes.Equality
import Vector


{-| Square Matrix type
-}
type SquareMatrix a
    = SquareMatrix (Matrix.Matrix a)


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { matrixSpace : Matrix.MatrixSpace a
    , innerProduct : SquareMatrix a -> SquareMatrix a -> Result String a
    , norm : SquareMatrix a -> Result String (Real.Real Float)
    , distance : SquareMatrix a -> SquareMatrix a -> Result String (Real.Real Float)
    }


{-| Create square Matrix with n dimension filled with zeros
-}
zeroSquareMatrix : Field.Field a -> Int -> SquareMatrix a
zeroSquareMatrix field dim =
    Matrix.zeros field dim dim
        |> SquareMatrix


{-| Dimension of the matrix
-}
dimension : SquareMatrix a -> Int
dimension (SquareMatrix matrix) =
    Matrix.mDimension matrix


{-| Determine whether a matirx is square
-}
isSquareMatrix : Matrix.Matrix a -> Bool
isSquareMatrix matrix =
    Matrix.mDimension matrix == Matrix.nDimension matrix


{-| Calculate the norm of a Matrix
-}
normReal : SquareMatrix (Real.Real Float) -> Result String (Real.Real Float)
normReal matrix =
    dotProduct Vector.realInnerProductSpace matrix matrix
        |> Result.map
            (Real.map Basics.sqrt)


{-| Calculate the norm of a Matrix
-}
normComplex : SquareMatrix (ComplexNumbers.ComplexNumber Float) -> Result String (Real.Real Float)
normComplex matrix =
    dotProduct Vector.complexInnerProductSpace matrix matrix
        |> Result.map
            (ComplexNumbers.real >> Real.map Basics.sqrt)


{-| Calculate distance between two vectors
-}
distanceReal : SquareMatrix (Real.Real Float) -> SquareMatrix (Real.Real Float) -> Result String (Real.Real Float)
distanceReal (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.realMatrixAdditionSemigroup matrixOne (Matrix.realMatrixAdditionGroup.inverse matrixTwo)
        |> SquareMatrix
        |> normReal


distanceComplex : SquareMatrix (ComplexNumbers.ComplexNumber Float) -> SquareMatrix (ComplexNumbers.ComplexNumber Float) -> Result String (Real.Real Float)
distanceComplex (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.complexMatrixAdditionSemigroup matrixOne (Matrix.complexMatrixAdditionGroup.inverse matrixTwo)
        |> SquareMatrix
        |> normComplex


{-| Predicate if matrix is right stochastic
-}
isRightStochastic : SquareMatrix (Real.Real Float) -> Bool
isRightStochastic (SquareMatrix (Matrix.Matrix listOfRowVectors)) =
    List.all
        (\(RowVector.RowVector vector) -> Real.equal.eq (Vector.sum Real.sumMonoid vector) Real.one)
        listOfRowVectors


{-| Predicate if matrix is left stochastic
-}
isLeftStochastic : SquareMatrix (Real.Real Float) -> Bool
isLeftStochastic (SquareMatrix matrix) =
    let
        (Matrix.Matrix transposedListOfRowVectors) =
            Matrix.transpose matrix
    in
    List.all
        (\(RowVector.RowVector vector) -> Real.equal.eq (Vector.sum Real.sumMonoid vector) Real.one)
        transposedListOfRowVectors


{-| Real Numbered Inner Product Space for Matrix
-}
realMatrixInnerProductSpace : InnerProductSpace (Real.Real Float)
realMatrixInnerProductSpace =
    { matrixSpace = Matrix.realMatrixSpace
    , innerProduct = dotProduct Vector.realInnerProductSpace
    , norm = normReal
    , distance = distanceReal
    }


{-| Complex Numbered Inner Product Space for Matrix
-}
complexMatrixInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumber Float)
complexMatrixInnerProductSpace =
    { matrixSpace = Matrix.complexMatrixSpace
    , innerProduct = dotProduct Vector.complexInnerProductSpace
    , norm = normComplex
    , distance = distanceComplex
    }


{-| Calculate the dot product of two Matricies
-}
dotProduct : Vector.InnerProductSpace a -> SquareMatrix a -> SquareMatrix a -> Result String a
dotProduct vectorInnerProductSpace (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    let
        productMatrix =
            Matrix.multiply vectorInnerProductSpace matrixOne matrixTwo
    in
    case productMatrix of
        Ok pMatrix ->
            if isSquareMatrix pMatrix then
                Matrix.getDiagonalProduct vectorInnerProductSpace.vectorSpace.field pMatrix
                    |> Result.fromMaybe "Index out of range"

            else
                Err "Must be Square Matrix"

        Err err ->
            Err err


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> SquareMatrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (SquareMatrix matrix) =
    Matrix.getAt ( rowIndex, columnIndex ) matrix


{-| Add two Square Matrices together
-}
add : Field.Field a -> SquareMatrix a -> SquareMatrix a -> SquareMatrix a
add field (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.add field matrixOne matrixTwo
        |> SquareMatrix


{-| Scalar multiplication over a Square Matrix
-}
scalarMultiplication : Field.Field a -> a -> SquareMatrix a -> SquareMatrix a
scalarMultiplication field scalar (SquareMatrix matrix) =
    Matrix.scalarMultiplication field scalar matrix
        |> SquareMatrix


{-| Square Matrix Square Matrix multiplication
-}
multiply :
    Vector.InnerProductSpace a
    -> SquareMatrix a
    -> SquareMatrix a
    -> Result String (SquareMatrix a)
multiply innerProductSpace (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.multiply innerProductSpace matrixOne matrixTwo
        |> Result.map SquareMatrix


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> SquareMatrix a
    -> ColumnVector.ColumnVector a
    -> Result String (ColumnVector.ColumnVector a)
multiplyMatrixVector innerProductSpace (SquareMatrix matrix) vector =
    Matrix.multiplyMatrixVector innerProductSpace matrix vector


{-| Create Square Identity Matrix with n dimension
-}
identity : Field.Field a -> Int -> SquareMatrix a
identity field =
    Matrix.identity field
        >> SquareMatrix


{-| Subtract two Square Matrices
-}
subtract : Field.Field a -> SquareMatrix a -> SquareMatrix a -> SquareMatrix a
subtract field (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.subtract field matrixOne matrixTwo
        |> SquareMatrix


{-| Perform the adjoint operation on a Complex Numbered Matrix
-}
adjoint :
    SquareMatrix (ComplexNumbers.ComplexNumber number)
    -> SquareMatrix (ComplexNumbers.ComplexNumber number)
adjoint (SquareMatrix matrix) =
    Matrix.adjoint matrix
        |> SquareMatrix


{-| Transpose a Matrix
-}
transpose : SquareMatrix a -> SquareMatrix a
transpose (SquareMatrix matrix) =
    Matrix.transpose matrix
        |> SquareMatrix


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : Vector.VectorSpace a -> SquareMatrix a -> SquareMatrix a
upperTriangle vectorSpace (SquareMatrix matrix) =
    Matrix.upperTriangle vectorSpace matrix
        |> SquareMatrix


{-| Get the Product of the diagonal of a Matrix
-}
getDiagonalProduct : Field.Field a -> SquareMatrix a -> Maybe a
getDiagonalProduct field (SquareMatrix matrix) =
    Matrix.getDiagonalProduct field matrix


{-| Append Matricies together horizontally
-}
appendHorizontal : SquareMatrix a -> SquareMatrix a -> SquareMatrix a
appendHorizontal (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.appendHorizontal matrixOne matrixTwo
        |> SquareMatrix


{-| Function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : Vector.VectorSpace a -> SquareMatrix a -> SquareMatrix a
gaussJordan vectorSpace (SquareMatrix matrix) =
    Matrix.gaussJordan vectorSpace matrix
        |> SquareMatrix


{-| Calculate the submatrix given a starting and ending row and column index
-}
subMatrix : Int -> Int -> Int -> Int -> SquareMatrix a -> SquareMatrix a
subMatrix startingRowIndex endingRowIndex startingColumnIndex endingColumnIndex (SquareMatrix matrix) =
    Matrix.subMatrix startingRowIndex endingRowIndex startingColumnIndex endingColumnIndex matrix
        |> SquareMatrix


{-| Create a Matrix from a list of Column Vectors
-}
createMatrixFromColumnVectors : List (ColumnVector.ColumnVector a) -> SquareMatrix a
createMatrixFromColumnVectors =
    Matrix.createMatrixFromColumnVectors
        >> SquareMatrix


{-| Compare two Matrices for equality
-}
equalImplementation : (a -> a -> Bool) -> SquareMatrix a -> SquareMatrix a -> Bool
equalImplementation comparator (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.equalImplementation comparator matrixOne matrixTwo


{-| Compare two matricies using comparator
-}
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (SquareMatrix a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Map over a Matrix
-}
map : (a -> b) -> SquareMatrix a -> SquareMatrix b
map f (SquareMatrix matrix) =
    Matrix.map f matrix
        |> SquareMatrix
