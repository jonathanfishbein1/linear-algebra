module Vector exposing
    ( Vector(..)
    , Vector3(..)
    , Scalar(..)
    , VectorSpace
    , InnerProductSpace
    , realVectorSpace
    , realVectorAbelianGroup
    , realInnerProductSpace
    , complexVectorSpace
    , complexVectorAbelianGroup
    , complexInnerProductSpace
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
    , cross
    , tensorProduct
    , distanceComplex
    , distanceReal
    , dimension
    , vectorSubspace
    , all
    , empty
    , append
    , concat
    , realVectorCommutativeSemigroup, complexVectorCommutativeSemigroup
    , realVectorCommutativeMonoid, complexVectorCommutativeMonoid
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
    , vector3ToVector
    , negativeOrPositiveFloat
    )

{-| A module for Vectors


# Types

@docs Vector
@docs Vector3
@docs Scalar
@docs VectorSpace
@docs InnerProductSpace


# Values

@docs realVectorSpace
@docs realVectorAbelianGroup
@docs realInnerProductSpace
@docs complexVectorSpace
@docs complexVectorAbelianGroup
@docs complexInnerProductSpace
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
@docs cross
@docs tensorProduct
@docs distanceComplex
@docs distanceReal


# Vector Properties

@docs dimension
@docs vectorSubspace
@docs all


# SemiGroup, Monoid, Group, Ring, Field

@docs empty
@docs append
@docs concat
@docs realVectorCommutativeSemigroup, complexVectorCommutativeSemigroup
@docs realVectorCommutativeMonoid, complexVectorCommutativeMonoid


# Functor, Applicative, Monad, Foldable

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
@docs negativeOrPositiveFloat

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


{-| 3 Dimensional Vector type
-}
type Vector3 a
    = Vector3 a a a


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
    , length : Vector a -> Float
    , distance : Vector a -> Vector a -> Float
    }


{-| Semigroup instance for a real valued Vector.
-}
realVectorSemigroup : Semigroup.Semigroup (Vector Float)
realVectorSemigroup =
    add Field.float


{-| Semigroup instance for a complex valued Vector.
-}
complexVectorSemigroup : Semigroup.Semigroup (Vector (ComplexNumbers.ComplexNumber Float))
complexVectorSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Vector.
-}
realVectorCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector Float)
realVectorCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realVectorSemigroup


{-| Commutative Semigroup instance for a complex valued Vector.
-}
complexVectorCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector (ComplexNumbers.ComplexNumber Float))
complexVectorCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexVectorSemigroup


{-| Monoid instance for a real valued Vector.
-}
realVectorMonoid : Monoid.Monoid (Vector Float)
realVectorMonoid =
    Monoid.semigroupAndIdentity realVectorSemigroup empty


{-| Monoid instance for a complex valued Vector.
-}
complexVectorMonoid : Monoid.Monoid (Vector (ComplexNumbers.ComplexNumber Float))
complexVectorMonoid =
    Monoid.semigroupAndIdentity complexVectorSemigroup empty


{-| Commutative Monoid instance for a real valued Vector.
-}
realVectorCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Vector Float)
realVectorCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realVectorMonoid


{-| Commutative Monoid instance for a complex valued Vector.
-}
complexVectorCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Vector (ComplexNumbers.ComplexNumber Float))
complexVectorCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexVectorMonoid


{-| Group instance for a real valued Vector.
-}
realVectorGroup : Group.Group (Vector Float)
realVectorGroup =
    { monoid = realVectorMonoid
    , inverse = map Group.numberSum.inverse
    }


{-| Group instance for a complex valued Vector.
-}
complexVectorGroup : Group.Group (Vector (ComplexNumbers.ComplexNumber Float))
complexVectorGroup =
    { monoid = complexVectorMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Vector.
-}
realVectorAbelianGroup : AbelianGroup.AbelianGroup (Vector Float)
realVectorAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realVectorMonoid
        , inverse = realVectorGroup.inverse
        }


{-| Group instance for a complex valued Vector.
-}
complexVectorAbelianGroup : AbelianGroup.AbelianGroup (Vector (ComplexNumbers.ComplexNumber Float))
complexVectorAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = complexVectorMonoid
        , inverse = complexVectorGroup.inverse
        }


{-| Real Numbered Vector Space
-}
realVectorSpace : VectorSpace Float
realVectorSpace =
    { abelianGroup = realVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Field.float
    , field = Field.float
    }


{-| Complex Numbered Vector Space
-}
complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumber Float)
complexVectorSpace =
    { abelianGroup = complexVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication ComplexNumbers.field
    , field = ComplexNumbers.field
    }


{-| Real Numbered Inner Product Space
-}
realInnerProductSpace : InnerProductSpace Float
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = dotProduct Field.float
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
lengthReal : Vector Float -> Float
lengthReal vector =
    dotProduct Field.float vector vector
        |> Basics.sqrt


{-| Calculate the length of a Complex valued Vector
-}
lengthComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Float
lengthComplex vector =
    dotProduct ComplexNumbers.field (conjugate vector) vector
        |> ComplexNumbers.real
        |> Basics.sqrt


{-| Adjust a real valued vector so that its length is exactly one
-}
normaliseReal : Vector Float -> Vector Float
normaliseReal v =
    if lengthReal v == 0 then
        v

    else
        scalarMultiplication Field.float (1 / lengthReal v) v


{-| Adjust a real valued vector so that its length is exactly one
-}
normaliseComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Vector (ComplexNumbers.ComplexNumber Float)
normaliseComplex v =
    if lengthComplex v == 0 then
        v

    else
        scalarMultiplication ComplexNumbers.field (ComplexNumbers.ComplexNumber (Real.Real (1 / lengthComplex v)) (Imaginary.Imaginary 0)) v


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
angleBetween : Vector Float -> Vector Float -> Float
angleBetween vectorOne vectorTwo =
    dotProduct Field.float vectorOne vectorTwo
        / (lengthReal vectorOne * lengthReal vectorTwo)
        |> Basics.acos


{-| Calculate the sum of a Vector
-}
sum : Monoid.Monoid a -> Vector a -> a
sum monoid (Vector vect) =
    monoid.concat vect


{-| Calculate distance between two vectors
-}
distanceReal : Vector Float -> Vector Float -> Float
distanceReal vectorOne vectorTwo =
    realVectorGroup.monoid.semigroup vectorOne (realVectorGroup.inverse vectorTwo)
        |> lengthReal


{-| Calculate distance between two vectors
-}
distanceComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Vector (ComplexNumbers.ComplexNumber Float) -> Float
distanceComplex vectorOne vectorTwo =
    complexVectorGroup.monoid.semigroup vectorOne (complexVectorGroup.inverse vectorTwo)
        |> lengthComplex


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


{-| Convert a Vector3 type to a Vector type
-}
vector3ToVector : Vector3 a -> Vector a
vector3ToVector (Vector3 x y z) =
    Vector [ x, y, z ]


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


{-| Print a Real Vector as a string
-}
printRealVector : Vector Float -> String
printRealVector vector =
    "Vector ["
        ++ (map String.fromFloat vector
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


float : Parser.Parser Float
float =
    Parser.number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }


{-| Parse a Float that can be negative or positive
-}
negativeOrPositiveFloat : Parser.Parser Float
negativeOrPositiveFloat =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= float
        , float
        ]


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
readRealVector : String -> Result (List Parser.DeadEnd) (Vector Float)
readRealVector =
    Parser.run (parseVector negativeOrPositiveFloat)


{-| Try to read a string into a Complex Vector
-}
readComplexVector :
    String
    -> Result (List Parser.DeadEnd) (Vector (ComplexNumbers.ComplexNumber Float))
readComplexVector =
    Parser.run (parseVector ComplexNumbers.parseComplexNumber)


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
