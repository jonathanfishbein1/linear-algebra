module ColumnVector exposing (ColumnVector(..))

import Vector


{-| Column Vector
-}
type ColumnVector a
    = ColumnVector (Vector.Vector a)
