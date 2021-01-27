module SquareMatrix exposing
    ( SquareMatrix(..)
    , InnerProductSpace
    , zeroSquareMatrix
    , realMatrixInnerProductSpace
    , isSymmetric
    , dimension
    , isSquareMatrix
    , normReal
    , normComplex
    , distanceReal
    , isRightStochastic
    , isLeftStochastic
    , scalarMultiplication
    , dotProduct
    , multiply
    , multiplyMatrixVector
    , add
    , getAt
    , complexMatrixInnerProductSpace
    )

{-| A module for Square Matrix


# Types

@docs SquareMatrix
@docs InnerProductSpace


# Values

@docs zeroSquareMatrix
@docs realMatrixInnerProductSpace


# Matrix Predicates and Properties

@docs isSymmetric
@docs dimension
@docs isSquareMatrix
@docs normReal
@docs normComplex
@docs distanceReal
@docs isRightStochastic
@docs isLeftStochastic


# Unitary Operations

@docs scalarMultiplication


# Binary Operations

@docs dotProduct
@docs multiply
@docs multiplyMatrixVector
@docs add


# Manipulation

@docs getAt

-}

import ComplexNumbers
import Field
import Float.Extra
import Matrix
import Monoid
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
    , norm : SquareMatrix a -> Result String Float
    , distance : SquareMatrix a -> SquareMatrix a -> Result String Float
    }


{-| Create square Matrix with n dimension filled with zeros
-}
zeroSquareMatrix : Field.Field a -> Int -> SquareMatrix a
zeroSquareMatrix field dim =
    Matrix.zeros field dim dim
        |> SquareMatrix


{-| Predicate to determine if Matrix is symmetric
-}
isSymmetric : SquareMatrix a -> Bool
isSymmetric (SquareMatrix matrix) =
    Matrix.transpose matrix == matrix


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
normReal : SquareMatrix Float -> Result String Float
normReal matrix =
    dotProduct Vector.realInnerProductSpace matrix matrix
        |> Result.map
            Basics.sqrt


{-| Calculate the norm of a Matrix
-}
normComplex : SquareMatrix (ComplexNumbers.ComplexNumber Float) -> Result String Float
normComplex matrix =
    dotProduct Vector.complexInnerProductSpace matrix matrix
        |> Result.map
            (ComplexNumbers.real >> Basics.sqrt)


{-| Calculate distance between two vectors
-}
distanceReal : SquareMatrix Float -> SquareMatrix Float -> Result String Float
distanceReal (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.realMatrixAdditionSemigroup matrixOne (Matrix.realMatrixAdditionGroup.inverse matrixTwo)
        |> SquareMatrix
        |> normReal


distanceComplex : SquareMatrix (ComplexNumbers.ComplexNumber Float) -> SquareMatrix (ComplexNumbers.ComplexNumber Float) -> Result String Float
distanceComplex (SquareMatrix matrixOne) (SquareMatrix matrixTwo) =
    Matrix.complexMatrixAdditionSemigroup matrixOne (Matrix.complexMatrixAdditionGroup.inverse matrixTwo)
        |> SquareMatrix
        |> normComplex


{-| Predicate if matrix is right stochastic
-}
isRightStochastic : SquareMatrix Float -> Bool
isRightStochastic (SquareMatrix (Matrix.Matrix listOfRowVectors)) =
    List.all
        (\(Matrix.RowVector vector) -> Float.Extra.equalWithin 1.0e-6 (Vector.sum Monoid.numberSum vector) 1)
        listOfRowVectors


{-| Predicate if matrix is left stochastic
-}
isLeftStochastic : SquareMatrix Float -> Bool
isLeftStochastic (SquareMatrix matrix) =
    let
        (Matrix.Matrix transposedListOfRowVectors) =
            Matrix.transpose matrix
    in
    List.all
        (\(Matrix.RowVector vector) -> Float.Extra.equalWithin 1.0e-6 (Vector.sum Monoid.numberSum vector) 1)
        transposedListOfRowVectors


{-| Real Numbered Inner Product Space for Matrix
-}
realMatrixInnerProductSpace : InnerProductSpace Float
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
    -> Vector.Vector a
    -> Result String (Vector.Vector a)
multiplyMatrixVector innerProductSpace (SquareMatrix matrix) vector =
    Matrix.multiplyMatrixVector innerProductSpace matrix vector
