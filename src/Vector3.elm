module Vector3 exposing
    ( Vector3(..)
    , cross, vector3ToVector
    )

{-| A module for Vectors


# Types

@docs Vector3

-}

import AbelianGroup
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers
import Field
import Group
import Imaginary
import List.Extra
import Monoid
import Parser exposing ((|.), (|=))
import Real
import Semigroup
import Typeclasses.Classes.Equality
import Vector


{-| 3 Dimensional Vector type
-}
type Vector3 a
    = Vector3 a a a


{-| Take the cross product of two 3D vectors
-}
cross : CommutativeDivisionRing.CommutativeDivisionRing a -> Vector3 a -> Vector3 a -> Vector3 a
cross (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    let
        (AbelianGroup.AbelianGroup groupAddition) =
            commutativeDivisionRing.addition
    in
    Vector3
        ((\x y -> groupAddition.monoid.semigroup x (groupAddition.inverse y)) (commutativeDivisionRing.multiplication.monoid.semigroup y1 z2) (commutativeDivisionRing.multiplication.monoid.semigroup y2 z1))
        ((\x y -> groupAddition.monoid.semigroup x (groupAddition.inverse y)) (commutativeDivisionRing.multiplication.monoid.semigroup z1 x2) (commutativeDivisionRing.multiplication.monoid.semigroup z2 x1))
        ((\x y -> groupAddition.monoid.semigroup x (groupAddition.inverse y)) (commutativeDivisionRing.multiplication.monoid.semigroup x1 y2) (commutativeDivisionRing.multiplication.monoid.semigroup x2 y1))


{-| Convert a Vector3 type to a Vector type
-}
vector3ToVector : Vector3 a -> Vector.Vector a
vector3ToVector (Vector3 x y z) =
    Vector.Vector [ x, y, z ]
