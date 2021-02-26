module Vector exposing
    ( Vector(..)
    , Scalar(..)
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

@docs Vector
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


# Vector Predicates and Properties

@docs dimension
@docs vectorSubspace
@docs all


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


# Monoid Functor, Applicative, Monad, Foldable functions

@docs empty
@docs append
@docs concat
@docs map
@docs pure
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
@docs vector3ToVector

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


{-| Type to represent a scalar value
-}
type Scalar a
    = Scalar a


{-| Vector type
-}
type Vector a
    = Vector (List a)


{-| Type to represent a Vector Space
-}
type alias VectorSpace a =
    { abelianGroup : AbelianGroup.AbelianGroup (Vector a)
    , vectorScalarMultiplication : a -> Vector a -> Vector a
    , field : Field.Field a
    }


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : Vector a -> Vector a -> a
    , length : Vector a -> Real.Real Float
    , distance : Vector a -> Vector a -> Real.Real Float
    }


{-| Semigroup instance for a real valued Vector.
-}
realSemigroup : Semigroup.Semigroup (Vector (Real.Real Float))
realSemigroup =
    add Real.field


{-| Semigroup instance for a complex valued Vector.
-}
complexSemigroup : Semigroup.Semigroup (Vector (ComplexNumbers.ComplexNumber Float))
complexSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Vector.
-}
realCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector (Real.Real Float))
realCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realSemigroup


{-| Commutative Semigroup instance for a complex valued Vector.
-}
complexCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector (ComplexNumbers.ComplexNumber Float))
complexCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSemigroup


{-| Monoid instance for a real valued Vector.
-}
realMonoid : Monoid.Monoid (Vector (Real.Real Float))
realMonoid =
    Monoid.semigroupAndIdentity realSemigroup empty


{-| Monoid instance for a complex valued Vector.
-}
complexMonoid : Monoid.Monoid (Vector (ComplexNumbers.ComplexNumber Float))
complexMonoid =
    Monoid.semigroupAndIdentity complexSemigroup empty


{-| Commutative Monoid instance for a real valued Vector.
-}
realCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Vector (Real.Real Float))
realCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMonoid


{-| Commutative Monoid instance for a complex valued Vector.
-}
complexCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Vector (ComplexNumbers.ComplexNumber Float))
complexCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMonoid


{-| Group instance for a real valued Vector.
-}
realGroup : Group.Group (Vector (Real.Real Float))
realGroup =
    { monoid = realMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for a complex valued Vector.
-}
complexGroup : Group.Group (Vector (ComplexNumbers.ComplexNumber Float))
complexGroup =
    { monoid = complexMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Vector.
-}
realAbelianGroup : AbelianGroup.AbelianGroup (Vector (Real.Real Float))
realAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMonoid
        , inverse = realGroup.inverse
        }


{-| Group instance for a complex valued Vector.
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (Vector (ComplexNumbers.ComplexNumber Float))
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


{-| Zero vector given a Field and dimension
-}
zeros : Monoid.Monoid a -> Int -> Vector a
zeros { identity } dim =
    List.repeat dim identity
        |> Vector


{-| Scalar multiplication over a Vector s
-}
scalarMultiplication : Field.Field a -> a -> Vector a -> Vector a
scalarMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) scalar =
    map (commutativeDivisionRing.multiplication.monoid.semigroup scalar)


{-| Calculate the length of a Real valued Vector
-}
lengthReal : Vector (Real.Real Float) -> Real.Real Float
lengthReal vector =
    dotProduct Real.field vector vector
        |> Real.real
        |> Basics.sqrt
        |> Real.Real


{-| Calculate the length of a Complex valued Vector
-}
lengthComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
lengthComplex vector =
    dotProduct ComplexNumbers.field (conjugate vector) vector
        |> ComplexNumbers.real
        |> Real.map Basics.sqrt


{-| Adjust a real valued vector so that its length is exactly one
-}
normaliseReal : Vector (Real.Real Float) -> Vector (Real.Real Float)
normaliseReal v =
    if lengthReal v == Real.zero then
        v

    else
        scalarMultiplication Real.field (Real.divide Real.one (lengthReal v)) v


{-| Adjust a real valued vector so that its length is exactly one
-}
normaliseComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Vector (ComplexNumbers.ComplexNumber Float)
normaliseComplex v =
    if Real.equal.eq (lengthComplex v) Real.zero then
        v

    else
        scalarMultiplication ComplexNumbers.field (ComplexNumbers.ComplexNumber (Real.divide Real.one (lengthComplex v)) Imaginary.zero) v


{-| Add two Vectors
-}
add : Field.Field a -> Vector a -> Vector a -> Vector a
add (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    map2 group.monoid.semigroup


{-| Subtract Vectors
-}
subtract : Field.Field a -> Vector a -> Vector a -> Vector a
subtract (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    add (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne (map group.inverse vectorTwo)


{-| Hadamard Multiplication Vectors
-}
hadamardMultiplication : Field.Field a -> Vector a -> Vector a -> Vector a
hadamardMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    map2 commutativeDivisionRing.multiplication.monoid.semigroup


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> Vector a -> Vector a -> a
dotProduct (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    hadamardMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) vectorOne vectorTwo
        |> sum group.monoid


{-| Calculate the angle between two vectors
-}
angleBetween : Vector (Real.Real Float) -> Vector (Real.Real Float) -> Real.Real Float
angleBetween vectorOne vectorTwo =
    Real.divide (dotProduct Real.field vectorOne vectorTwo)
        (Real.multiply (lengthReal vectorOne) (lengthReal vectorTwo))
        |> Real.map Basics.acos


{-| Calculate the sum of a Vector
-}
sum : Monoid.Monoid a -> Vector a -> a
sum monoid (Vector vect) =
    monoid.concat vect


{-| Calculate distance between two vectors
-}
distanceReal : Vector (Real.Real Float) -> Vector (Real.Real Float) -> Real.Real Float
distanceReal vectorOne vectorTwo =
    realGroup.monoid.semigroup vectorOne (realGroup.inverse vectorTwo)
        |> lengthReal


{-| Calculate distance between two vectors
-}
distanceComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Vector (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
distanceComplex vectorOne vectorTwo =
    complexGroup.monoid.semigroup vectorOne (complexGroup.inverse vectorTwo)
        |> lengthComplex


{-| Calculate the tensor product of two vectors
-}
tensorProduct : Field.Field a -> Vector a -> Vector a -> Vector a
tensorProduct field vectorOne vectorTwo =
    andThen
        (\vectorOneElement ->
            scalarMultiplication field vectorOneElement vectorTwo
        )
        vectorOne


{-| Map over a vector
-}
map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


{-| Lift a binary function to work with Vectors
-}
map2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c
map2 f (Vector vectorOne) (Vector vectorTwo) =
    List.map2 f vectorOne vectorTwo
        |> Vector


{-| Place a value in minimal Vector context
-}
pure : a -> Vector a
pure a =
    Vector [ a ]


{-| Apply for Vector
-}
andMap : Vector a -> Vector (a -> b) -> Vector b
andMap vector fVector =
    map2 Basics.identity fVector vector


{-| andThen for Vector
-}
andThen : (a -> Vector b) -> Vector a -> Vector b
andThen fVector (Vector list) =
    List.concatMap
        (\x ->
            let
                (Vector result) =
                    fVector x
            in
            result
        )
        list
        |> Vector


{-| Left fold over a Vector
-}
foldl : (a -> b -> b) -> b -> Vector a -> b
foldl foldFunction acc (Vector list) =
    List.foldl foldFunction acc list


{-| Monoid empty for Vector
-}
empty : Vector a
empty =
    Vector []


{-| Append Vectors together
-}
append : Vector a -> Vector a -> Vector a
append (Vector listOne) (Vector listTwo) =
    listOne
        ++ listTwo
        |> Vector


{-| Monoidally append Vectors together
-}
concat : Monoid.Monoid (Vector a)
concat =
    Monoid.semigroupAndIdentity
        append
        empty


{-| Count of number of elements in a vector
-}
dimension : Vector a -> Int
dimension (Vector list) =
    List.length list


{-| Determine whether a list of Vectors makes a Subspace
-}
vectorSubspace :
    Field.Field a
    -> AbelianGroup.AbelianGroup (Vector a)
    -> Scalar a
    -> List (Vector a)
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
            List.map (\(Vector vector) -> Vector <| List.map2 Basics.identity predicates vector)
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
all : (a -> Bool) -> Vector a -> Bool
all predicate (Vector list) =
    List.all predicate list


{-| Compare two Vectors for equality
-}
equalImplementation : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equalImplementation comparator vectorOne vectorTwo =
    map2 comparator vectorOne vectorTwo
        |> all ((==) True)


{-| `Equal` type for `Vector`.
-}
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (Vector a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Get the value in a Vector at the specified index
-}
getAt : Int -> Vector a -> Maybe a
getAt index (Vector list) =
    List.Extra.getAt index list


{-| Set the value in a Vector at the specified index
-}
setAt : Int -> a -> Vector a -> Vector a
setAt index element (Vector list) =
    List.Extra.setAt index element list
        |> Vector


{-| Find index of a value in a Vector
-}
findIndex : (a -> Bool) -> Vector a -> Maybe Int
findIndex predicate (Vector list) =
    List.Extra.findIndex predicate list


{-| Take the complex conjugate of a Complex Numbered Vector
-}
conjugate :
    Vector (ComplexNumbers.ComplexNumber number)
    -> Vector (ComplexNumbers.ComplexNumber number)
conjugate =
    map ComplexNumbers.conjugate


{-| Print a Real Vector as a string
-}
printRealVector : Vector (Real.Real Float) -> String
printRealVector vector =
    "Vector ["
        ++ (map Real.print vector
                |> (\(Vector list) -> list)
                |> String.join ", "
           )
        ++ "]"


{-| Print a Complex Vector as a string
-}
printComplexVector : Vector (ComplexNumbers.ComplexNumber Float) -> String
printComplexVector vector =
    "Vector ["
        ++ (map ComplexNumbers.print vector
                |> (\(Vector list) -> list)
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


{-| Parse a Vector
-}
parseVector : Parser.Parser a -> Parser.Parser (Vector a)
parseVector vectorElementsParser =
    Parser.succeed Vector
        |. Parser.keyword "Vector"
        |. Parser.spaces
        |= listParser vectorElementsParser


{-| Try to read a string into a Real Vector
-}
readRealVector : String -> Result (List Parser.DeadEnd) (Vector (Real.Real Float))
readRealVector =
    Parser.run (parseVector Real.parseReal)


{-| Try to read a string into a Complex Vector
-}
readComplexVector :
    String
    -> Result (List Parser.DeadEnd) (Vector (ComplexNumbers.ComplexNumber Float))
readComplexVector =
    Parser.run (parseVector ComplexNumbers.parseComplexNumber)
