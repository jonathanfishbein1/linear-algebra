module ColumnVector exposing
    ( ColumnVector(..)
    , sum
    )

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
