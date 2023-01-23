module Internal.Vector exposing
    ( Scalar(..)
    , VectorSpace
    , InnerProductSpace
    , zeros
    , scalarMultiplication
    , lengthReal
    , lengthComplex
    , sum
    , normaliseReal
    , normaliseComplex
    , conjugate
    , add
    , subtract
    , hadamardMultiplication
    , dotProduct
    , angleBetween
    , tensorProduct
    , distanceComplex
    , distanceReal
    , dimension
    , vectorSubspace
    , all
    , count
    , realCommutativeSemigroup
    , complexCommutativeSemigroup
    , realCommutativeMonoid
    , complexCommutativeMonoid
    , realVectorSpace
    , complexVectorSpace
    , realAbelianGroup
    , complexAbelianGroup
    , realInnerProductSpace
    , complexInnerProductSpace
    , empty
    , append
    , concat
    , map
    , pure
    , andMapZip
    , andMap
    , map2
    , andThen
    , foldl
    , equal
    , equalImplementation
    , findIndex
    , getAt
    , setAt
    , parseVector
    , printRealVector
    , printComplexVector
    , readRealVector
    , readComplexVector
    )

{-| A module for Vectors


# Types

@docs Scalar
@docs VectorSpace
@docs InnerProductSpace


# Values

@docs zeros


# Unitary Operations

@docs scalarMultiplication
@docs lengthReal
@docs lengthComplex
@docs sum
@docs normaliseReal
@docs normaliseComplex
@docs conjugate


# Binary Operations

@docs add
@docs subtract
@docs hadamardMultiplication
@docs dotProduct
@docs angleBetween
@docs tensorProduct
@docs distanceComplex
@docs distanceReal


# Vector.Vector Predicates and Properties

@docs dimension
@docs vectorSubspace
@docs all
@docs count


# SemiGroup, Monoid, Group, Ring, Field instances

@docs realCommutativeSemigroup
@docs complexCommutativeSemigroup
@docs realCommutativeMonoid
@docs complexCommutativeMonoid
@docs realVectorSpace
@docs complexVectorSpace
@docs realAbelianGroup
@docs complexAbelianGroup
@docs realInnerProductSpace
@docs complexInnerProductSpace


# Monoid, Functor, Applicative, Monad, Foldable functions

@docs empty
@docs append
@docs concat
@docs map
@docs pure
@docs andMapZip
@docs andMap
@docs map2
@docs andThen
@docs foldl


# Equality

@docs equal
@docs equalImplementation


# Manipulation

@docs findIndex
@docs getAt
@docs setAt
@docs parseVector
@docs printRealVector
@docs printComplexVector
@docs readRealVector
@docs readComplexVector

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


{-| Type to represent a scalar value
-}
type Scalar a
    = Scalar a


{-| Type to represent a Vector.Vector Space
-}
type alias VectorSpace a =
    { abelianGroup : AbelianGroup.AbelianGroup (Vector.Vector a)
    , vectorScalarMultiplication : a -> Vector.Vector a -> Vector.Vector a
    , field : Field.Field a
    }


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : Vector.Vector a -> Vector.Vector a -> a
    , length : Vector.Vector a -> Real.Real Float
    , distance : Vector.Vector a -> Vector.Vector a -> Real.Real Float
    }


{-| Semigroup instance for a real valued Vector.Vector.
-}
realSemigroup : Semigroup.Semigroup (Vector.Vector (Real.Real Float))
realSemigroup =
    add Real.field


{-| Semigroup instance for a complex valued Vector.Vector.
-}
complexSemigroup : Semigroup.Semigroup (Vector.Vector (ComplexNumbers.ComplexNumber Float))
complexSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Vector.Vector.
-}
realCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector.Vector (Real.Real Float))
realCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realSemigroup


{-| Commutative Semigroup instance for a complex valued Vector.Vector.
-}
complexCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector.Vector (ComplexNumbers.ComplexNumber Float))
complexCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSemigroup


{-| Monoid instance for a real valued Vector.Vector.
-}
realMonoid : Monoid.Monoid (Vector.Vector (Real.Real Float))
realMonoid =
    Monoid.semigroupAndIdentity realSemigroup empty


{-| Monoid instance for a complex valued Vector.Vector.
-}
complexMonoid : Monoid.Monoid (Vector.Vector (ComplexNumbers.ComplexNumber Float))
complexMonoid =
    Monoid.semigroupAndIdentity complexSemigroup empty


{-| Commutative Monoid instance for a real valued Vector.Vector.
-}
realCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Vector.Vector (Real.Real Float))
realCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMonoid


{-| Commutative Monoid instance for a complex valued Vector.Vector.
-}
complexCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Vector.Vector (ComplexNumbers.ComplexNumber Float))
complexCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMonoid


{-| Group instance for a real valued Vector.Vector.
-}
realGroup : Group.Group (Vector.Vector (Real.Real Float))
realGroup =
    { monoid = realMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for a complex valued Vector.Vector.
-}
complexGroup : Group.Group (Vector.Vector (ComplexNumbers.ComplexNumber Float))
complexGroup =
    { monoid = complexMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Vector.Vector.
-}
realAbelianGroup : AbelianGroup.AbelianGroup (Vector.Vector (Real.Real Float))
realAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMonoid
        , inverse = realGroup.inverse
        }


{-| Group instance for a complex valued Vector.Vector.
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (Vector.Vector (ComplexNumbers.ComplexNumber Float))
complexAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = complexMonoid
        , inverse = complexGroup.inverse
        }


{-| Real Numbered Vector.Vector Space
-}
realVectorSpace : VectorSpace (Real.Real Float)
realVectorSpace =
    { abelianGroup = realAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Real.field
    , field = Real.field
    }


{-| Complex Numbered Vector.Vector Space
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


{-| Zero vector given a Field and dimension
-}
zeros : Monoid.Monoid a -> Int -> Vector.Vector a
zeros { identity } dim =
    List.repeat dim identity
        |> Vector.Vector


{-| Scalar multiplication over a Vector.Vector s
-}
scalarMultiplication : Field.Field a -> a -> Vector.Vector a -> Vector.Vector a
scalarMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) scalar =
    map (commutativeDivisionRing.multiplication.monoid.semigroup scalar)


{-| Calculate the length of a Real valued Vector.Vector
-}
lengthReal : Vector.Vector (Real.Real Float) -> Real.Real Float
lengthReal vector =
    dotProduct Real.field vector vector
        |> Real.real
        |> Basics.sqrt
        |> Real.Real


{-| Calculate the length of a Complex valued Vector.Vector
-}
lengthComplex : Vector.Vector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
lengthComplex vector =
    dotProduct ComplexNumbers.field (conjugate vector) vector
        |> ComplexNumbers.real
        |> Real.map Basics.sqrt


{-| Adjust a real valued vector so that its length is exactly one
-}
normaliseReal : Vector.Vector (Real.Real Float) -> Vector.Vector (Real.Real Float)
normaliseReal v =
    if lengthReal v == Real.zero then
        v

    else
        scalarMultiplication Real.field (Real.divide Real.one (lengthReal v)) v


{-| Adjust a real valued vector so that its length is exactly one
-}
normaliseComplex : Vector.Vector (ComplexNumbers.ComplexNumber Float) -> Vector.Vector (ComplexNumbers.ComplexNumber Float)
normaliseComplex v =
    if Real.equal.eq (lengthComplex v) Real.zero then
        v

    else
        scalarMultiplication ComplexNumbers.field (ComplexNumbers.ComplexNumber (Real.divide Real.one (lengthComplex v)) Imaginary.zero) v


{-| Add two Vectors
-}
add : Field.Field a -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
add (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    map2 group.monoid.semigroup


{-| Subtract Vectors
-}
subtract : Field.Field a -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
subtract (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    add (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne (map group.inverse vectorTwo)


{-| Hadamard Multiplication Vectors
-}
hadamardMultiplication : Field.Field a -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
hadamardMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    map2 commutativeDivisionRing.multiplication.monoid.semigroup


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> Vector.Vector a -> Vector.Vector a -> a
dotProduct (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    hadamardMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo
        |> sum group.monoid


{-| Calculate the angle between two vectors
-}
angleBetween : Vector.Vector (Real.Real Float) -> Vector.Vector (Real.Real Float) -> Real.Real Float
angleBetween vectorOne vectorTwo =
    Real.divide (dotProduct Real.field vectorOne vectorTwo)
        (Real.multiply (lengthReal vectorOne) (lengthReal vectorTwo))
        |> Real.map Basics.acos


{-| Calculate the sum of a Vector.Vector
-}
sum : Monoid.Monoid a -> Vector.Vector a -> a
sum monoid (Vector.Vector vect) =
    monoid.concat vect


{-| Calculate distance between two vectors
-}
distanceReal : Vector.Vector (Real.Real Float) -> Vector.Vector (Real.Real Float) -> Real.Real Float
distanceReal vectorOne vectorTwo =
    realGroup.monoid.semigroup vectorOne (realGroup.inverse vectorTwo)
        |> lengthReal


{-| Calculate distance between two vectors
-}
distanceComplex : Vector.Vector (ComplexNumbers.ComplexNumber Float) -> Vector.Vector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
distanceComplex vectorOne vectorTwo =
    complexGroup.monoid.semigroup vectorOne (complexGroup.inverse vectorTwo)
        |> lengthComplex


{-| Calculate the tensor product of two vectors
-}
tensorProduct : Field.Field a -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
tensorProduct (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo =
    andMap vectorTwo (map commutativeDivisionRing.multiplication.monoid.semigroup vectorOne)


{-| Map over a vector
-}
map : (a -> b) -> Vector.Vector a -> Vector.Vector b
map f (Vector.Vector vector) =
    Vector.Vector <| List.map f vector


{-| Lift a binary function to work with Vectors
-}
map2 : (a -> b -> c) -> Vector.Vector a -> Vector.Vector b -> Vector.Vector c
map2 f (Vector.Vector vectorOne) (Vector.Vector vectorTwo) =
    List.map2 f vectorOne vectorTwo
        |> Vector.Vector


{-| Place a value in minimal Vector.Vector context
-}
pure : a -> Vector.Vector a
pure a =
    Vector.Vector [ a ]


{-| Apply for Vector.Vector using zip like implementation
-}
andMapZip : Vector.Vector a -> Vector.Vector (a -> b) -> Vector.Vector b
andMapZip vector fVector =
    map2 Basics.identity fVector vector


{-| Apply for Vector.Vector using Cartesian product like implementation
-}
andMap : Vector.Vector a -> Vector.Vector (a -> b) -> Vector.Vector b
andMap vectorOne fVector =
    andThen
        (\func ->
            map func vectorOne
        )
        fVector


{-| andThen for Vector.Vector
-}
andThen : (a -> Vector.Vector b) -> Vector.Vector a -> Vector.Vector b
andThen fReturnVector (Vector.Vector list) =
    List.concatMap
        (\x ->
            let
                (Vector.Vector result) =
                    fReturnVector x
            in
            result
        )
        list
        |> Vector.Vector


{-| Left fold over a Vector.Vector
-}
foldl : (a -> b -> b) -> b -> Vector.Vector a -> b
foldl foldFunction acc (Vector.Vector list) =
    List.foldl foldFunction acc list


{-| Monoid empty for Vector.Vector
-}
empty : Vector.Vector a
empty =
    Vector.Vector []


{-| Append Vectors together
-}
append : Vector.Vector a -> Vector.Vector a -> Vector.Vector a
append (Vector.Vector listOne) (Vector.Vector listTwo) =
    listOne
        ++ listTwo
        |> Vector.Vector


{-| Monoidally append Vectors together
-}
concat : Monoid.Monoid (Vector.Vector a)
concat =
    Monoid.semigroupAndIdentity
        append
        empty


{-| Count of number of elements in a vector
-}
dimension : Vector.Vector a -> Int
dimension (Vector.Vector list) =
    List.length list


{-| Determine whether a list of Vectors makes a Subspace
-}
vectorSubspace :
    Field.Field a
    -> AbelianGroup.AbelianGroup (Vector.Vector a)
    -> Scalar a
    -> List (Vector.Vector a)
    -> List (a -> Bool)
    -> Bool
vectorSubspace (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) (AbelianGroup.AbelianGroup vectorGroup) (Scalar scalar) vectorList predicates =
    let
        (AbelianGroup.AbelianGroup additionGroup) =
            commutativeDivisionRing.addition

        testzeros =
            List.map (scalarMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) additionGroup.monoid.identity) vectorList

        containszeros =
            closurePassCriteria testzeros

        scaledVectors =
            List.map (scalarMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) scalar) vectorList

        closurePassCriteria =
            List.map (\(Vector.Vector vector) -> Vector.Vector <| List.map2 Basics.identity predicates vector)
                >> List.all (all ((==) True))

        closureUnderScalarMultiplication =
            closurePassCriteria scaledVectors

        cartesianAddVectors =
            List.Extra.lift2 vectorGroup.monoid.semigroup

        additionOfVectors =
            cartesianAddVectors vectorList vectorList

        closureUnderAddition =
            closurePassCriteria additionOfVectors
    in
    containszeros && closureUnderScalarMultiplication && closureUnderAddition


{-| Determine if all elements in a vector satisfy some test
-}
all : (a -> Bool) -> Vector.Vector a -> Bool
all predicate (Vector.Vector list) =
    List.all predicate list


{-| Compare two Vectors for equality
-}
equalImplementation : (a -> a -> Bool) -> Vector.Vector a -> Vector.Vector a -> Bool
equalImplementation comparator vectorOne vectorTwo =
    map2 comparator vectorOne vectorTwo
        |> all ((==) True)


{-| `Equal` type for `Vector.Vector`.
-}
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (Vector.Vector a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Get the value in a Vector.Vector at the specified index
-}
getAt : Int -> Vector.Vector a -> Maybe a
getAt index (Vector.Vector list) =
    List.Extra.getAt index list


{-| Set the value in a Vector.Vector at the specified index
-}
setAt : Int -> a -> Vector.Vector a -> Vector.Vector a
setAt index element (Vector.Vector list) =
    List.Extra.setAt index element list
        |> Vector.Vector


{-| Find index of a value in a Vector.Vector
-}
findIndex : (a -> Bool) -> Vector.Vector a -> Maybe Int
findIndex predicate (Vector.Vector list) =
    List.Extra.findIndex predicate list


{-| Take the complex conjugate of a Complex Numbered Vector.Vector
-}
conjugate :
    Vector.Vector (ComplexNumbers.ComplexNumber number)
    -> Vector.Vector (ComplexNumbers.ComplexNumber number)
conjugate =
    map ComplexNumbers.conjugate


{-| Print a Real Vector.Vector as a string
-}
printRealVector : Vector.Vector (Real.Real Float) -> String
printRealVector vector =
    "Vector.Vector ["
        ++ (map Real.print vector
                |> (\(Vector.Vector list) -> list)
                |> String.join ", "
           )
        ++ "]"


{-| Print a Complex Vector.Vector as a string
-}
printComplexVector : Vector.Vector (ComplexNumbers.ComplexNumber Float) -> String
printComplexVector vector =
    "Vector.Vector ["
        ++ (map ComplexNumbers.print vector
                |> (\(Vector.Vector list) -> list)
                |> String.join ", "
           )
        ++ "]"


listParser : Parser.Parser a -> Parser.Parser (List a)
listParser itemParser =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = itemParser
        , trailing = Parser.Forbidden
        }


{-| Parse a Vector.Vector
-}
parseVector : Parser.Parser a -> Parser.Parser (Vector.Vector a)
parseVector vectorElementsParser =
    Parser.succeed Vector.Vector
        |. Parser.keyword "Vector.Vector"
        |. Parser.spaces
        |= listParser vectorElementsParser


{-| Try to read a string into a Real Vector.Vector
-}
readRealVector : String -> Result (List Parser.DeadEnd) (Vector.Vector (Real.Real Float))
readRealVector =
    Parser.run (parseVector Real.parseReal)


{-| Try to read a string into a Complex Vector.Vector
-}
readComplexVector :
    String
    -> Result (List Parser.DeadEnd) (Vector.Vector (ComplexNumbers.ComplexNumber Float))
readComplexVector =
    Parser.run (parseVector ComplexNumbers.parseComplexNumber)


{-| Count the number of elements in a Vector.Vector that satisfy the given condition
-}
count : (a -> Bool) -> Vector.Vector a -> Int
count condition (Vector.Vector list) =
    List.Extra.count condition list
