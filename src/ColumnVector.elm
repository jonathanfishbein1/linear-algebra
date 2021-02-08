module ColumnVector exposing
    ( ColumnVector(..)
    , sum
    , scalarMultiplication
    , conjugate
    , add
    , dimension
    , map
    , foldl
    , equal
    , getAt
    )

{-| A module for Column Vector


# Types

@docs ColumnVector


# Unitary Operations

@docs sum
@docs scalarMultiplication
@docs conjugate


# Binary Operations

@docs add


# ColumnVector Properties

@docs dimension


# Functor, Applicative, Monad, Foldable

@docs map
@docs foldl


# Equality

@docs equal


# Manipulation

@docs getAt

-}

import ComplexNumbers
import Field
import Monoid
import Vector


{-| Column Vector
-}
type ColumnVector a
    = ColumnVector (Vector.Vector a)


{-| Calculate the sum of a Vector
-}
sum : Monoid.Monoid a -> ColumnVector a -> a
sum monoid (ColumnVector vect) =
    Vector.sum monoid vect


{-| Add two ColumnVectors
-}
add : Field.Field a -> ColumnVector a -> ColumnVector a -> ColumnVector a
add field (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Vector.add field vectorOne vectorTwo
        |> ColumnVector


{-| Scalar multiplication over a ColumnVector
-}
scalarMultiplication : Field.Field a -> a -> ColumnVector a -> ColumnVector a
scalarMultiplication field scalar (ColumnVector vector) =
    Vector.scalarMultiplication field scalar vector
        |> ColumnVector


{-| map over a RowVector
-}
map : (a -> b) -> ColumnVector a -> ColumnVector b
map f (ColumnVector vector) =
    Vector.map f vector
        |> ColumnVector


{-| Left fold over a RowVector
-}
foldl : (a -> b -> b) -> b -> ColumnVector a -> b
foldl foldFunction acc (ColumnVector vector) =
    Vector.foldl foldFunction acc vector


{-| Compare two vectors for equality using a comparator
-}
equal : (a -> a -> Bool) -> ColumnVector a -> ColumnVector a -> Bool
equal comparator (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Vector.equal comparator vectorOne vectorTwo


{-| Get the value in a Vector at the specified index
-}
getAt : Int -> ColumnVector a -> Maybe a
getAt index (ColumnVector list) =
    Vector.getAt index list


{-| Count of number of elements in a Ket
-}
dimension : ColumnVector a -> Int
dimension (ColumnVector vector) =
    Vector.dimension vector


{-| Take the complex conjugate of a Complex Numbered ColumnVector
-}
conjugate :
    ColumnVector (ComplexNumbers.ComplexNumber number)
    -> ColumnVector (ComplexNumbers.ComplexNumber number)
conjugate (ColumnVector vector) =
    Vector.conjugate vector
        |> ColumnVector
