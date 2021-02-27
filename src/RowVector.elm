module RowVector exposing
    ( RowVector(..)
    , all
    , map
    , map2
    , foldl
    , parseRowVector
    , InnerProductSpace, VectorSpace, complexInnerProductSpace, complexVectorSpace, empty, findIndex, getAt, realInnerProductSpace, realVectorSpace, scalarMultiplication
    )

{-| A module for Row Vector


# Types

@docs RowVector


# Vector Properties

@docs all


# Functor, Applicative, Monad, Foldable

@docs map
@docs map2
@docs foldl


# Manipulation

@docs parseRowVector

-}

import AbelianGroup
import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers
import Field
import Group
import Monoid
import Parser exposing ((|.), (|=))
import Real
import Semigroup
import Vector


{-| Row Vector
-}
type RowVector a
    = RowVector (Vector.Vector a)


{-| Type to represent a Vector Space
-}
type alias VectorSpace a =
    { abelianGroup : AbelianGroup.AbelianGroup (RowVector a)
    , vectorScalarMultiplication : a -> RowVector a -> RowVector a
    , field : Field.Field a
    }


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : RowVector a -> RowVector a -> a
    , length : RowVector a -> Real.Real Float
    , distance : RowVector a -> RowVector a -> Real.Real Float
    }


{-| Semigroup instance for a real valued Vector.
-}
realSemigroup : Semigroup.Semigroup (RowVector (Real.Real Float))
realSemigroup =
    add Real.field


{-| Semigroup instance for a complex valued Vector.
-}
complexSemigroup : Semigroup.Semigroup (RowVector (ComplexNumbers.ComplexNumber Float))
complexSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Vector.
-}
realCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (RowVector (Real.Real Float))
realCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realSemigroup


{-| Commutative Semigroup instance for a complex valued Vector.
-}
complexCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (RowVector (ComplexNumbers.ComplexNumber Float))
complexCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSemigroup


{-| Monoid instance for a real valued Vector.
-}
realMonoid : Monoid.Monoid (RowVector (Real.Real Float))
realMonoid =
    Monoid.semigroupAndIdentity realSemigroup empty


{-| Monoid instance for a complex valued Vector.
-}
complexMonoid : Monoid.Monoid (RowVector (ComplexNumbers.ComplexNumber Float))
complexMonoid =
    Monoid.semigroupAndIdentity complexSemigroup empty


{-| Commutative Monoid instance for a real valued Vector.
-}
realCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (RowVector (Real.Real Float))
realCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMonoid


{-| Commutative Monoid instance for a complex valued Vector.
-}
complexCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (RowVector (ComplexNumbers.ComplexNumber Float))
complexCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMonoid


{-| Group instance for a real valued Vector.
-}
realGroup : Group.Group (RowVector (Real.Real Float))
realGroup =
    { monoid = realMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for a complex valued Vector.
-}
complexGroup : Group.Group (RowVector (ComplexNumbers.ComplexNumber Float))
complexGroup =
    { monoid = complexMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Vector.
-}
realAbelianGroup : AbelianGroup.AbelianGroup (RowVector (Real.Real Float))
realAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMonoid
        , inverse = realGroup.inverse
        }


{-| Group instance for a complex valued Vector.
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (RowVector (ComplexNumbers.ComplexNumber Float))
complexAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = complexMonoid
        , inverse = complexGroup.inverse
        }


{-| Real Numbered Vector Space
-}
realVectorSpace : VectorSpace (Real.Real Float)
realVectorSpace =
    { abelianGroup = realAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Real.field
    , field = Real.field
    }


{-| Complex Numbered Vector Space
-}
complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumber Float)
complexVectorSpace =
    { abelianGroup = complexAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication ComplexNumbers.field
    , field = ComplexNumbers.field
    }


{-| Real Numbered Inner Product Space
-}
realInnerProductSpace : InnerProductSpace (Real.Real Float)
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = dotProduct Real.field
    , length = lengthReal
    , distance = distanceReal
    }


{-| Complex Numbered Inner Product Space
-}
complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumber Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = dotProduct ComplexNumbers.field
    , length = lengthComplex
    , distance = distanceComplex
    }


{-| Add two Vectors
-}
add : Field.Field a -> RowVector a -> RowVector a -> RowVector a
add field (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.add field vectorOne vectorTwo
        |> RowVector


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> RowVector a -> RowVector a -> a
dotProduct field (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.dotProduct field vectorOne vectorTwo


{-| map over a RowVector
-}
map : (a -> b) -> RowVector a -> RowVector b
map f (RowVector vector) =
    Vector.map f vector
        |> RowVector


{-| map2 over a RowVector
-}
map2 : (a -> b -> c) -> RowVector a -> RowVector b -> RowVector c
map2 f (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.map2 f vectorOne vectorTwo
        |> RowVector


{-| Left fold over a RowVector
-}
foldl : (a -> b -> b) -> b -> RowVector a -> b
foldl foldFunction acc (RowVector vector) =
    Vector.foldl foldFunction acc vector


{-| Predicate to determine if all values in the RowVector satisfy the given predicate
-}
all : (a -> Bool) -> RowVector a -> Bool
all predicate (RowVector vector) =
    Vector.all predicate vector


{-| Parse a RowVector
-}
parseRowVector : Parser.Parser a -> Parser.Parser (RowVector a)
parseRowVector rowVectorElementsParser =
    Parser.succeed RowVector
        |. Parser.keyword "RowVector"
        |. Parser.spaces
        |= Vector.parseVector rowVectorElementsParser


{-| Get the value in a Vector at the specified index
-}
getAt : Int -> RowVector a -> Maybe a
getAt index (RowVector list) =
    Vector.getAt index list


{-| Monoid empty for RowVector
-}
empty : RowVector a
empty =
    Vector.empty
        |> RowVector


{-| Find index of a value in a Vector
-}
findIndex : (a -> Bool) -> RowVector a -> Maybe Int
findIndex predicate (RowVector list) =
    Vector.findIndex predicate list


{-| Scalar multiplication over a RowVector
-}
scalarMultiplication : Field.Field a -> a -> RowVector a -> RowVector a
scalarMultiplication field scalar (RowVector vector) =
    Vector.scalarMultiplication field scalar vector
        |> RowVector


{-| Calculate the length of a Real valued Vector
-}
lengthReal : RowVector (Real.Real Float) -> Real.Real Float
lengthReal (RowVector vector) =
    Vector.lengthReal vector


{-| Calculate the length of a Complex valued Vector
-}
lengthComplex : RowVector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
lengthComplex (RowVector vector) =
    Vector.lengthComplex vector


{-| Calculate distance between two vectors
-}
distanceReal : RowVector (Real.Real Float) -> RowVector (Real.Real Float) -> Real.Real Float
distanceReal (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.distanceReal vectorOne vectorTwo


{-| Calculate distance between two vectors
-}
distanceComplex : RowVector (ComplexNumbers.ComplexNumber Float) -> RowVector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
distanceComplex (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.distanceComplex vectorOne vectorTwo
