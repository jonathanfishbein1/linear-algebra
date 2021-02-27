module RowVector exposing
    ( RowVector(..)
    , VectorSpace
    , InnerProductSpace
    , scalarMultiplication
    , sum
    , dimension
    , all
    , count
    , realVectorSpace
    , complexVectorSpace
    , realInnerProductSpace
    , complexInnerProductSpace
    , empty
    , append
    , pure
    , map
    , map2
    , foldl
    , findIndex
    , getAt
    , setAt
    , parseRowVector
    , printRealRowVectorList
    , printComplexRowVectorList
    )

{-| A module for Row Vector


# Types

@docs RowVector
@docs VectorSpace
@docs InnerProductSpace


# Unitary Operations

@docs scalarMultiplication
@docs sum


# RowVector Predicates and Properties

@docs dimension
@docs all
@docs count


# SemiGroup, Monoid, Group, Ring, Field instances

@docs realVectorSpace
@docs complexVectorSpace
@docs realInnerProductSpace
@docs complexInnerProductSpace


# Monoid, Functor, Applicative, Monad, Foldable

@docs empty
@docs append
@docs pure
@docs map
@docs map2
@docs foldl


# Manipulation

@docs findIndex
@docs getAt
@docs setAt
@docs parseRowVector
@docs printRealRowVectorList
@docs printComplexRowVectorList

-}

import AbelianGroup
import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers
import Field
import Group
import Internal.Vector
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


{-| Semigroup instance for a real valued Internal.Vector.
-}
realSemigroup : Semigroup.Semigroup (RowVector (Real.Real Float))
realSemigroup =
    add Real.field


{-| Semigroup instance for a complex valued Internal.Vector.
-}
complexSemigroup : Semigroup.Semigroup (RowVector (ComplexNumbers.ComplexNumber Float))
complexSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Internal.Vector.
-}
realCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (RowVector (Real.Real Float))
realCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realSemigroup


{-| Commutative Semigroup instance for a complex valued Internal.Vector.
-}
complexCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (RowVector (ComplexNumbers.ComplexNumber Float))
complexCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSemigroup


{-| Monoid instance for a real valued Internal.Vector.
-}
realMonoid : Monoid.Monoid (RowVector (Real.Real Float))
realMonoid =
    Monoid.semigroupAndIdentity realSemigroup empty


{-| Monoid instance for a complex valued Internal.Vector.
-}
complexMonoid : Monoid.Monoid (RowVector (ComplexNumbers.ComplexNumber Float))
complexMonoid =
    Monoid.semigroupAndIdentity complexSemigroup empty


{-| Commutative Monoid instance for a real valued Internal.Vector.
-}
realCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (RowVector (Real.Real Float))
realCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMonoid


{-| Commutative Monoid instance for a complex valued Internal.Vector.
-}
complexCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (RowVector (ComplexNumbers.ComplexNumber Float))
complexCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMonoid


{-| Group instance for a real valued Internal.Vector.
-}
realGroup : Group.Group (RowVector (Real.Real Float))
realGroup =
    { monoid = realMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for a complex valued Internal.Vector.
-}
complexGroup : Group.Group (RowVector (ComplexNumbers.ComplexNumber Float))
complexGroup =
    { monoid = complexMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Internal.Vector.
-}
realAbelianGroup : AbelianGroup.AbelianGroup (RowVector (Real.Real Float))
realAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMonoid
        , inverse = realGroup.inverse
        }


{-| Group instance for a complex valued Internal.Vector.
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
    Internal.Vector.add field vectorOne vectorTwo
        |> RowVector


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> RowVector a -> RowVector a -> a
dotProduct field (RowVector vectorOne) (RowVector vectorTwo) =
    Internal.Vector.dotProduct field vectorOne vectorTwo


{-| map over a RowVector
-}
map : (a -> b) -> RowVector a -> RowVector b
map f (RowVector vector) =
    Internal.Vector.map f vector
        |> RowVector


{-| map2 over a RowVector
-}
map2 : (a -> b -> c) -> RowVector a -> RowVector b -> RowVector c
map2 f (RowVector vectorOne) (RowVector vectorTwo) =
    Internal.Vector.map2 f vectorOne vectorTwo
        |> RowVector


{-| Left fold over a RowVector
-}
foldl : (a -> b -> b) -> b -> RowVector a -> b
foldl foldFunction acc (RowVector vector) =
    Internal.Vector.foldl foldFunction acc vector


{-| Predicate to determine if all values in the RowVector satisfy the given predicate
-}
all : (a -> Bool) -> RowVector a -> Bool
all predicate (RowVector vector) =
    Internal.Vector.all predicate vector


{-| Parse a RowVector
-}
parseRowVector : Parser.Parser a -> Parser.Parser (RowVector a)
parseRowVector rowVectorElementsParser =
    Parser.succeed RowVector
        |. Parser.keyword "RowVector"
        |. Parser.spaces
        |= Internal.Vector.parseVector rowVectorElementsParser


{-| Get the value in a Vector at the specified index
-}
getAt : Int -> RowVector a -> Maybe a
getAt index (RowVector list) =
    Internal.Vector.getAt index list


{-| Monoid empty for RowVector
-}
empty : RowVector a
empty =
    Internal.Vector.empty
        |> RowVector


{-| Find index of a value in a Vector
-}
findIndex : (a -> Bool) -> RowVector a -> Maybe Int
findIndex predicate (RowVector list) =
    Internal.Vector.findIndex predicate list


{-| Scalar multiplication over a RowVector
-}
scalarMultiplication : Field.Field a -> a -> RowVector a -> RowVector a
scalarMultiplication field scalar (RowVector vector) =
    Internal.Vector.scalarMultiplication field scalar vector
        |> RowVector


{-| Calculate the length of a Real valued Vector
-}
lengthReal : RowVector (Real.Real Float) -> Real.Real Float
lengthReal (RowVector vector) =
    Internal.Vector.lengthReal vector


{-| Calculate the length of a Complex valued Vector
-}
lengthComplex : RowVector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
lengthComplex (RowVector vector) =
    Internal.Vector.lengthComplex vector


{-| Calculate distance between two vectors
-}
distanceReal : RowVector (Real.Real Float) -> RowVector (Real.Real Float) -> Real.Real Float
distanceReal (RowVector vectorOne) (RowVector vectorTwo) =
    Internal.Vector.distanceReal vectorOne vectorTwo


{-| Calculate distance between two vectors
-}
distanceComplex : RowVector (ComplexNumbers.ComplexNumber Float) -> RowVector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
distanceComplex (RowVector vectorOne) (RowVector vectorTwo) =
    Internal.Vector.distanceComplex vectorOne vectorTwo


{-| Count of number of elements in a vector
-}
dimension : RowVector a -> Int
dimension (RowVector vector) =
    Internal.Vector.dimension vector


{-| Append Vectors together
-}
append : RowVector a -> RowVector a -> RowVector a
append (RowVector vectorOne) (RowVector vectorTwo) =
    Internal.Vector.append vectorOne vectorTwo
        |> RowVector


{-| Set the value in a Vector at the specified index
-}
setAt : Int -> a -> RowVector a -> RowVector a
setAt index element (RowVector vector) =
    Internal.Vector.setAt index element vector
        |> RowVector


{-| Print a Real RowVector to a string
-}
printRealRowVector : RowVector (Real.Real Float) -> String
printRealRowVector (RowVector vector) =
    "RowVector " ++ Internal.Vector.printRealVector vector ++ " ]"


{-| Print a Complex RowVector to a string
-}
printComplexRowVector : RowVector (ComplexNumbers.ComplexNumber Float) -> String
printComplexRowVector (RowVector vector) =
    "RowVector " ++ Internal.Vector.printComplexVector vector ++ " ]"


{-| Print a Real RowVector List to a string
-}
printRealRowVectorList : List (RowVector (Real.Real Float)) -> String
printRealRowVectorList listOfRowVectors =
    List.foldl
        (\row acc -> printRealRowVector row ++ acc)
        ""
        listOfRowVectors


{-| Print a Complex RowVector List to a string
-}
printComplexRowVectorList : List (RowVector (ComplexNumbers.ComplexNumber Float)) -> String
printComplexRowVectorList listOfRowVectors =
    List.foldl
        (\row acc -> printComplexRowVector row ++ acc)
        ""
        listOfRowVectors


{-| Count the number of elements in a RowVector that satisfy the given condition
-}
count : (a -> Bool) -> RowVector a -> Int
count condition (RowVector vector) =
    Internal.Vector.count condition vector


{-| Place a value in minimal RowVector context
-}
pure : a -> RowVector a
pure a =
    Internal.Vector.pure a
        |> RowVector


{-| Calculate the sum of a RowVector
-}
sum : Monoid.Monoid a -> RowVector a -> a
sum monoid (RowVector vect) =
    Internal.Vector.sum monoid vect
