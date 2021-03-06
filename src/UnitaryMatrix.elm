module UnitaryMatrix exposing
    ( UnitaryMatrix(..)
    , empty
    , identity
    , isUnitary
    , dimension
    , scalarMultiplication
    , multiply
    , multiplyIfCan
    , multiplyMatrixVector
    , getAt
    , setAt
    , equal
    )

{-| A module for Unitary Matrix


# Types

@docs UnitaryMatrix


# Values

@docs empty


# Constructors

@docs identity


# Matrix Predicates and Properties

@docs isUnitary
@docs dimension


# Unitary Operations

@docs scalarMultiplication


# Binary Operations

@docs multiply
@docs multiplyIfCan
@docs multiplyMatrixVector


# Manipulation

@docs getAt
@docs setAt


# Equality

@docs equal

-}

import ColumnVector
import ComplexNumbers
import InvertableMatrix
import RowVector
import Typeclasses.Classes.Equality


{-| Unitary Matrix type
-}
type UnitaryMatrix number
    = UnitaryMatrix (InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber number))


{-| Determine whether a matirx is unitary
-}
isUnitary : InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber Float) -> Result String (InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber Float))
isUnitary matrix =
    InvertableMatrix.invert RowVector.complexInnerProductSpace matrix
        |> Result.andThen (\inverse -> multiplyIfCan (UnitaryMatrix inverse) (UnitaryMatrix matrix))
        |> (\resultMatrix ->
                case resultMatrix of
                    Ok resultM ->
                        if equal.eq resultM (identity (dimension resultM)) then
                            Ok matrix

                        else
                            Err "Inverse of A multiplied by A does not equal the Identity matrix"

                    Err error ->
                        Err error
           )


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


{-| Set the value in a Normal Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> ComplexNumbers.ComplexNumber number -> UnitaryMatrix number -> UnitaryMatrix number
setAt tup element (UnitaryMatrix matrix) =
    InvertableMatrix.setAt tup element matrix
        |> UnitaryMatrix


{-| Square Matrix Square Matrix multiplication
-}
multiply :
    UnitaryMatrix Float
    -> UnitaryMatrix Float
    -> UnitaryMatrix Float
multiply (UnitaryMatrix matrixOne) (UnitaryMatrix matrixTwo) =
    InvertableMatrix.multiply RowVector.complexInnerProductSpace matrixOne matrixTwo
        |> UnitaryMatrix


{-| Unitary Matrix Unitary Matrix multiplication
-}
multiplyIfCan :
    UnitaryMatrix Float
    -> UnitaryMatrix Float
    -> Result String (UnitaryMatrix Float)
multiplyIfCan (UnitaryMatrix matrixOne) (UnitaryMatrix matrixTwo) =
    InvertableMatrix.multiplyIfCan RowVector.complexInnerProductSpace matrixOne matrixTwo
        |> Result.map UnitaryMatrix


{-| Multiply a ColumnVector by a Unitary Matrix
-}
multiplyMatrixVector :
    UnitaryMatrix Float
    -> ColumnVector.ColumnVector (ComplexNumbers.ComplexNumber Float)
    -> Result String (ColumnVector.ColumnVector (ComplexNumbers.ComplexNumber Float))
multiplyMatrixVector (UnitaryMatrix matrix) vector =
    InvertableMatrix.multiplyMatrixVector RowVector.complexInnerProductSpace matrix vector


{-| Compare two Matrices for equality
-}
equalImplementation : UnitaryMatrix Float -> UnitaryMatrix Float -> Bool
equalImplementation (UnitaryMatrix matrixOne) (UnitaryMatrix matrixTwo) =
    InvertableMatrix.equalImplementation ComplexNumbers.equal.eq matrixOne matrixTwo


{-| Compare two matricies using comparator
-}
equal : Typeclasses.Classes.Equality.Equality (UnitaryMatrix Float)
equal =
    Typeclasses.Classes.Equality.eq equalImplementation


{-| Create Square Identity Matrix with n dimension
-}
identity : Int -> UnitaryMatrix Float
identity =
    InvertableMatrix.identity ComplexNumbers.field
        >> UnitaryMatrix


{-| Scalar multiplication over an InvertableMatrix Matrix
-}
scalarMultiplication : ComplexNumbers.ComplexNumber Float -> UnitaryMatrix Float -> UnitaryMatrix Float
scalarMultiplication scalar (UnitaryMatrix matrix) =
    InvertableMatrix.scalarMultiplication ComplexNumbers.field scalar matrix
        |> UnitaryMatrix


{-| Monoid empty for UnitaryMatrix
-}
empty : UnitaryMatrix Float
empty =
    InvertableMatrix.empty
        |> UnitaryMatrix
