module ColumnVector exposing
    ( ColumnVector(..)
    , sum
    , scalarMultiplication
    , conjugate
    , lengthReal
    , lengthComplex
    , normaliseReal
    , normaliseComplex
    , add
    , dotProduct
    , distanceReal
    , distanceComplex
    , dimension
    , empty
    , map
    , foldl
    , equal
    , getAt
    , setAt
    )

{-| A module for Column Vectors


# Types

@docs ColumnVector


# Unitary Operations

@docs sum
@docs scalarMultiplication
@docs conjugate
@docs lengthReal
@docs lengthComplex
@docs normaliseReal
@docs normaliseComplex


# Binary Operations

@docs add
@docs dotProduct
@docs distanceReal
@docs distanceComplex


# ColumnVector Properties

@docs dimension


# Semigroup, Functor, Applicative, Monad, Foldable

@docs empty
@docs map
@docs foldl


# Equality

@docs equal


# Manipulation

@docs getAt
@docs setAt

-}

import AbelianGroup
import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers
import Field
import Group
import Internal.Vector
import Monoid
import Real
import Semigroup
import Typeclasses.Classes.Equality
import Vector


{-| Column Vector
-}
type ColumnVector a
    = ColumnVector (Vector.Vector a)


{-| Type to represent a Vector Space
-}
type alias VectorSpace a =
    { abelianGroup : AbelianGroup.AbelianGroup (ColumnVector a)
    , vectorScalarMultiplication : a -> ColumnVector a -> ColumnVector a
    , field : Field.Field a
    }


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : ColumnVector a -> ColumnVector a -> a
    , length : ColumnVector a -> Real.Real Float
    , distance : ColumnVector a -> ColumnVector a -> Real.Real Float
    }


{-| Semigroup instance for a real valued Internal.Vector.
-}
realSemigroup : Semigroup.Semigroup (ColumnVector (Real.Real Float))
realSemigroup =
    add Real.field


{-| Semigroup instance for a complex valued Internal.Vector.
-}
complexSemigroup : Semigroup.Semigroup (ColumnVector (ComplexNumbers.ComplexNumber Float))
complexSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Internal.Vector.
-}
realCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (ColumnVector (Real.Real Float))
realCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realSemigroup


{-| Commutative Semigroup instance for a complex valued Internal.Vector.
-}
complexCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (ColumnVector (ComplexNumbers.ComplexNumber Float))
complexCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSemigroup


{-| Monoid instance for a real valued Internal.Vector.
-}
realMonoid : Monoid.Monoid (ColumnVector (Real.Real Float))
realMonoid =
    Monoid.semigroupAndIdentity realSemigroup empty


{-| Monoid instance for a complex valued Internal.Vector.
-}
complexMonoid : Monoid.Monoid (ColumnVector (ComplexNumbers.ComplexNumber Float))
complexMonoid =
    Monoid.semigroupAndIdentity complexSemigroup empty


{-| Commutative Monoid instance for a real valued Internal.Vector.
-}
realCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (ColumnVector (Real.Real Float))
realCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMonoid


{-| Commutative Monoid instance for a complex valued Internal.Vector.
-}
complexCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (ColumnVector (ComplexNumbers.ComplexNumber Float))
complexCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMonoid


{-| Group instance for a real valued Internal.Vector.
-}
realGroup : Group.Group (ColumnVector (Real.Real Float))
realGroup =
    { monoid = realMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for a complex valued Internal.Vector.
-}
complexGroup : Group.Group (ColumnVector (ComplexNumbers.ComplexNumber Float))
complexGroup =
    { monoid = complexMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Internal.Vector.
-}
realAbelianGroup : AbelianGroup.AbelianGroup (ColumnVector (Real.Real Float))
realAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMonoid
        , inverse = realGroup.inverse
        }


{-| Group instance for a complex valued Internal.Vector.
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (ColumnVector (ComplexNumbers.ComplexNumber Float))
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


{-| Monoid empty for Vector
-}
empty : ColumnVector a
empty =
    Vector.Vector []
        |> ColumnVector


{-| Calculate the sum of a Vector
-}
sum : Monoid.Monoid a -> ColumnVector a -> a
sum monoid (ColumnVector vect) =
    Internal.Vector.sum monoid vect


{-| Add two ColumnVectors
-}
add : Field.Field a -> ColumnVector a -> ColumnVector a -> ColumnVector a
add field (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Internal.Vector.add field vectorOne vectorTwo
        |> ColumnVector


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> ColumnVector a -> ColumnVector a -> a
dotProduct field (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Internal.Vector.dotProduct field vectorOne vectorTwo


{-| Scalar multiplication over a ColumnVector
-}
scalarMultiplication : Field.Field a -> a -> ColumnVector a -> ColumnVector a
scalarMultiplication field scalar (ColumnVector vector) =
    Internal.Vector.scalarMultiplication field scalar vector
        |> ColumnVector


{-| Calculate distance between two vectors
-}
distanceReal : ColumnVector (Real.Real Float) -> ColumnVector (Real.Real Float) -> Real.Real Float
distanceReal (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Internal.Vector.distanceReal vectorOne vectorTwo


{-| Calculate distance between two vectors
-}
distanceComplex : ColumnVector (ComplexNumbers.ComplexNumber Float) -> ColumnVector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
distanceComplex (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Internal.Vector.distanceComplex vectorOne vectorTwo


{-| Calculate the length of a Real valued Vector
-}
lengthReal : ColumnVector (Real.Real Float) -> Real.Real Float
lengthReal (ColumnVector vector) =
    Internal.Vector.lengthReal vector


{-| Calculate the length of a Complex valued Vector
-}
lengthComplex : ColumnVector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
lengthComplex (ColumnVector vector) =
    Internal.Vector.lengthComplex vector


{-| map over a RowVector
-}
map : (a -> b) -> ColumnVector a -> ColumnVector b
map f (ColumnVector vector) =
    Internal.Vector.map f vector
        |> ColumnVector


{-| Left fold over a RowVector
-}
foldl : (a -> b -> b) -> b -> ColumnVector a -> b
foldl foldFunction acc (ColumnVector vector) =
    Internal.Vector.foldl foldFunction acc vector


{-| Compare two Vectors for equality
-}
equalImplementation : (a -> a -> Bool) -> ColumnVector a -> ColumnVector a -> Bool
equalImplementation comparator (ColumnVector vectorOne) (ColumnVector vectorTwo) =
    Internal.Vector.equalImplementation comparator vectorOne vectorTwo


{-| `Equal` type for `Vector`.
-}
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (ColumnVector a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Get the value in a Column Vector at the specified index
-}
getAt : Int -> ColumnVector a -> Maybe a
getAt index (ColumnVector list) =
    Internal.Vector.getAt index list


{-| Set the value in a Column Vector at the specified index
-}
setAt : Int -> a -> ColumnVector a -> ColumnVector a
setAt index element (ColumnVector list) =
    Internal.Vector.setAt index element list
        |> ColumnVector


{-| Count of number of elements in a Ket
-}
dimension : ColumnVector a -> Int
dimension (ColumnVector vector) =
    Internal.Vector.dimension vector


{-| Take the complex conjugate of a Complex Numbered ColumnVector
-}
conjugate :
    ColumnVector (ComplexNumbers.ComplexNumber number)
    -> ColumnVector (ComplexNumbers.ComplexNumber number)
conjugate (ColumnVector vector) =
    Internal.Vector.conjugate vector
        |> ColumnVector


{-| Adjust a real valued column vector so that its length is exactly one
-}
normaliseReal : ColumnVector (Real.Real Float) -> ColumnVector (Real.Real Float)
normaliseReal (ColumnVector v) =
    Internal.Vector.normaliseReal v
        |> ColumnVector


{-| Adjust a complex valued column vector so that its length is exactly one
-}
normaliseComplex : ColumnVector (ComplexNumbers.ComplexNumber Float) -> ColumnVector (ComplexNumbers.ComplexNumber Float)
normaliseComplex (ColumnVector v) =
    Internal.Vector.normaliseComplex v
        |> ColumnVector
