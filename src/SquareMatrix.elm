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
    , dotProduct
    , getAt
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


# Binary Operations

@docs dotProduct


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
    , innerProduct : Matrix.Matrix a -> Matrix.Matrix a -> Result String Float
    , norm : Matrix.Matrix a -> Result String Float
    , distance : Matrix.Matrix a -> Matrix.Matrix a -> Result String Float
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
normReal : Matrix.Matrix Float -> Result String Float
normReal matrix =
    dotProduct Vector.realInnerProductSpace matrix matrix
        |> Result.map
            Basics.sqrt


{-| Calculate the norm of a Matrix
-}
normComplex : Matrix.Matrix (ComplexNumbers.ComplexNumber Float) -> Result String Float
normComplex matrix =
    dotProduct Vector.complexInnerProductSpace matrix matrix
        |> Result.map
            (ComplexNumbers.real >> Basics.sqrt)


{-| Calculate distance between two vectors
-}
distanceReal : Matrix.Matrix Float -> Matrix.Matrix Float -> Result String Float
distanceReal matrixOne matrixTwo =
    Matrix.realMatrixAdditionSemigroup matrixOne (Matrix.realMatrixAdditionGroup.inverse matrixTwo)
        |> normReal


{-| Predicate if matrix is right stochastic
-}
isRightStochastic : Matrix.Matrix Float -> Bool
isRightStochastic (Matrix.Matrix listOfRowVectors) =
    if isSquareMatrix (Matrix.Matrix listOfRowVectors) then
        List.all
            (\(Matrix.RowVector vector) -> Float.Extra.equalWithin 1.0e-6 (Vector.sum Monoid.numberSum vector) 1)
            listOfRowVectors

    else
        False


{-| Predicate if matrix is left stochastic
-}
isLeftStochastic : Matrix.Matrix Float -> Bool
isLeftStochastic matrix =
    let
        (Matrix.Matrix transposedListOfRowVectors) =
            Matrix.transpose matrix
    in
    if isSquareMatrix (Matrix.Matrix transposedListOfRowVectors) then
        List.all
            (\(Matrix.RowVector vector) -> Float.Extra.equalWithin 1.0e-6 (Vector.sum Monoid.numberSum vector) 1)
            transposedListOfRowVectors

    else
        False


{-| Real Numbered Inner Product Space for Matrix
-}
realMatrixInnerProductSpace : InnerProductSpace Float
realMatrixInnerProductSpace =
    { matrixSpace = Matrix.realMatrixSpace
    , innerProduct = dotProduct Vector.realInnerProductSpace
    , norm = normReal
    , distance = distanceReal
    }


{-| Calculate the dot product of two Matricies
-}
dotProduct : Vector.InnerProductSpace a -> Matrix.Matrix a -> Matrix.Matrix a -> Result String a
dotProduct vectorInnerProductSpace matrixOne matrixTwo =
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
