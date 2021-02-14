module UnitaryMatrix exposing
    ( UnitaryMatrix(..)
    , identity
    , isUnitary
    , dimension
    , multiply
    , multiplyMatrixVector
    , getAt
    , equal
    )

{-| A module for Unitary Matrix


# Types

@docs UnitaryMatrix


# Constructors

@docs identity


# Matrix Predicates and Properties

@docs isUnitary
@docs dimension


# Binary Operations

@docs multiply
@docs multiplyMatrixVector


# Manipulation

@docs getAt


# Equality

@docs equal

-}

import ColumnVector
import ComplexNumbers
import InvertableMatrix
import Typeclasses.Classes.Equality
import Vector


{-| Unitary Matrix type
-}
type UnitaryMatrix number
    = UnitaryMatrix (InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber number))


{-| Determine whether a matirx is unitary
-}
isUnitary : InvertableMatrix.InvertableMatrix (ComplexNumbers.ComplexNumber Float) -> Bool
isUnitary matrix =
    InvertableMatrix.invert Vector.complexInnerProductSpace matrix
        |> Result.andThen (\inverse -> multiply (UnitaryMatrix inverse) (UnitaryMatrix matrix))
        |> (\resultMatrix ->
                case resultMatrix of
                    Ok resultM ->
                        (equal ComplexNumbers.equal.eq).eq resultM (identity (dimension resultM))

                    Err _ ->
                        False
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
    -> ColumnVector.ColumnVector (ComplexNumbers.ComplexNumber Float)
    -> Result String (ColumnVector.ColumnVector (ComplexNumbers.ComplexNumber Float))
multiplyMatrixVector (UnitaryMatrix matrix) vector =
    InvertableMatrix.multiplyMatrixVector Vector.complexInnerProductSpace matrix vector


{-| Compare two Matrices for equality
-}
equalImplementation : (ComplexNumbers.ComplexNumber number -> ComplexNumbers.ComplexNumber number -> Bool) -> UnitaryMatrix number -> UnitaryMatrix number -> Bool
equalImplementation comparator (UnitaryMatrix matrixOne) (UnitaryMatrix matrixTwo) =
    InvertableMatrix.equalImplementation comparator matrixOne matrixTwo


{-| Compare two matricies using comparator
-}
equal : (ComplexNumbers.ComplexNumber number -> ComplexNumbers.ComplexNumber number -> Bool) -> Typeclasses.Classes.Equality.Equality (UnitaryMatrix number)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Create Square Identity Matrix with n dimension
-}
identity : Int -> UnitaryMatrix Float
identity =
    InvertableMatrix.identity ComplexNumbers.field
        >> UnitaryMatrix
