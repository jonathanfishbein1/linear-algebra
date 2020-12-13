module Vector exposing
    ( Vector(..)
    , Vector3(..)
    , Scalar(..)
    , AbelianGroup
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
    , sum
    , addVectors
    , subtractVectors
    , hadamardMultiplication
    , dotProduct
    , angleBetween
    , cross
    , tensorProduct
    , dimension
    , vectorSubspace
    , all
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
    , distanceComplex, distanceReal, lengthReal, normaliseReal
    )

{-| A module for Vectors


# Types

@docs Vector
@docs Vector3
@docs Scalar
@docs AbelianGroup
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
@docs length
@docs normalise
@docs sum


# Binary Operations

@docs addVectors
@docs subtractVectors
@docs hadamardMultiplication
@docs dotProduct
@docs angleBetween
@docs cross
@docs distance
@docs tensorProduct


# Vector Properties

@docs dimension
@docs vectorSubspace
@docs all


# Monoid

@docs empty
@docs append
@docs concat


# Functor, Applicative, Monad, Foldable

@docs map
@docs pure
@docs andMap
@docs map2
@docs andThen
@docs foldl


# Equality

@docs equal


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

import AbelianGroup exposing (AbelianGroup)
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeRing
import CommutativeSemigroup
import ComplexNumbers
import Field
import Group
import List.Extra
import Monoid
import Parser exposing ((|.), (|=))
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


{-| Type to represent a Abelian Group
-}
type alias AbelianGroup a =
    { field : Field.Field a
    , addVects : Vector a -> Vector a -> Vector a
    , inverse : Vector a -> Vector a
    }


{-| Type to represent a Vector Space
-}
type alias VectorSpace a =
    { abelianGroup : AbelianGroup a
    , vectorScalarMultiplication : a -> Vector a -> Vector a
    }


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : Vector a -> Vector a -> a
    , length : Vector a -> Float
    , distance : Vector a -> Vector a -> Float
    }


{-| Instance for Vector under the addition operation.
-}
vectorAdditionSemigroup : Semigroup.Semigroup (Vector Float)
vectorAdditionSemigroup =
    addVectors Field.numberField


{-| Instance for Vector under the addition operation.
-}
vectorAdditionCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Vector Float)
vectorAdditionCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup vectorAdditionSemigroup


{-| Real Numbered Abelian Group
-}
realVectorAbelianGroup : AbelianGroup Float
realVectorAbelianGroup =
    { field = Field.numberField
    , addVects = addVectors Field.numberField
    , inverse = map Group.numberSum.inverse
    }


{-| Complex Numbered Abelian Group
-}
complexVectorAbelianGroup : AbelianGroup (ComplexNumbers.ComplexNumber Float)
complexVectorAbelianGroup =
    { field = ComplexNumbers.complexField
    , addVects = addVectors ComplexNumbers.complexField
    , inverse = map ComplexNumbers.complexSumGroup.inverse
    }


{-| Real Numbered Vector Space
-}
realVectorSpace : VectorSpace Float
realVectorSpace =
    { abelianGroup = realVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Field.numberField
    }


{-| Complex Numbered Vector Space
-}
complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumber Float)
complexVectorSpace =
    { abelianGroup = complexVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication ComplexNumbers.complexField
    }


{-| Real Numbered Inner Product Space
-}
realInnerProductSpace : InnerProductSpace Float
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = dotProduct Field.numberField
    , length = lengthReal
    , distance = distanceReal
    }


{-| Complex Numbered Inner Product Space
-}
complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumber Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = dotProduct ComplexNumbers.complexField
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
scalarMultiplication (Field.Field field) scalar =
    let
        (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) =
            field

        group =
            commutativeDivisionRing.multiplication

        semigroup =
            group.monoid.semigroup
    in
    map (semigroup scalar)


{-| Calculate the length of a Vector
-}
lengthReal : Vector Float -> Float
lengthReal vector =
    dotProduct Field.numberField vector vector
        |> Basics.sqrt


{-| Calculate the length of a Vector
-}
lengthComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Float
lengthComplex vector =
    dotProduct ComplexNumbers.complexField vector vector
        |> ComplexNumbers.modulus
        |> Basics.sqrt


{-| Adjust a vector so that its length is exactly one
-}
normaliseReal : Vector Float -> Vector Float
normaliseReal v =
    if lengthReal v == 0 then
        v

    else
        map ((*) (1 / lengthReal v)) v


{-| Add two Vectors
-}
addVectors : Field.Field a -> Vector a -> Vector a -> Vector a
addVectors (Field.Field field) =
    let
        (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) =
            field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition

        monoid =
            group.monoid

        semigroup =
            group.monoid.semigroup
    in
    map2 semigroup


{-| Subtract Vectors
-}
subtractVectors : Field.Field a -> Vector a -> Vector a -> Vector a
subtractVectors (Field.Field field) vectorOne vectorTwo =
    let
        (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) =
            field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    addVectors (Field.Field field) vectorOne (map group.inverse vectorTwo)


{-| Hadamard Multiplication Vectors
-}
hadamardMultiplication : Field.Field a -> Vector a -> Vector a -> Vector a
hadamardMultiplication (Field.Field field) =
    let
        (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) =
            field

        group =
            commutativeDivisionRing.multiplication

        semigroup =
            group.monoid.semigroup
    in
    map2 semigroup


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> Vector a -> Vector a -> a
dotProduct (Field.Field field) vectorOne vectorTwo =
    let
        (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) =
            field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    hadamardMultiplication (Field.Field field) vectorOne vectorTwo
        |> sum group.monoid


{-| Calculate the angle between two vectors
-}
angleBetween : Vector Float -> Vector Float -> Float
angleBetween vectorOne vectorTwo =
    let
        dotP =
            dotProduct Field.numberField vectorOne vectorTwo

        lengthVectorOne =
            lengthReal vectorOne

        lengthVectorTwo =
            lengthReal vectorTwo
    in
    Basics.acos (dotP / (lengthVectorOne * lengthVectorTwo))


{-| Calculate the sum of a Vector
-}
sum : Monoid.Monoid a -> Vector a -> a
sum monoid =
    foldl monoid.semigroup monoid.identity


{-| Calculate distance between two vectors
-}
distanceReal : Vector Float -> Vector Float -> Float
distanceReal vectorOne vectorTwo =
    realVectorSpace.abelianGroup.addVects vectorOne (realVectorSpace.abelianGroup.inverse vectorTwo)
        |> lengthReal


{-| Calculate distance between two vectors
-}
distanceComplex : Vector (ComplexNumbers.ComplexNumber Float) -> Vector (ComplexNumbers.ComplexNumber Float) -> Float
distanceComplex vectorOne vectorTwo =
    complexVectorSpace.abelianGroup.addVects vectorOne (complexVectorSpace.abelianGroup.inverse vectorTwo)
        |> lengthComplex


{-| Take the cross product of two 3D vectors
-}
cross : CommutativeDivisionRing.CommutativeDivisionRing a -> Vector3 a -> Vector3 a -> Vector3 a
cross (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    let
        (AbelianGroup.AbelianGroup groupAddition) =
            commutativeDivisionRing.addition

        additionMonoid =
            groupAddition.monoid

        additionSemigroup =
            additionMonoid.semigroup

        multiplicationGroup =
            commutativeDivisionRing.multiplication

        multiplicationSemigroup =
            multiplicationGroup.monoid.semigroup
    in
    Vector3
        ((\x y -> additionSemigroup x (groupAddition.inverse y)) (multiplicationSemigroup y1 z2) (multiplicationSemigroup y2 z1))
        ((\x y -> additionSemigroup x (groupAddition.inverse y)) (multiplicationSemigroup z1 x2) (multiplicationSemigroup z2 x1))
        ((\x y -> additionSemigroup x (groupAddition.inverse y)) (multiplicationSemigroup x1 y2) (multiplicationSemigroup x2 y1))


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
    AbelianGroup a
    -> Scalar a
    -> List (Vector a)
    -> List (a -> Bool)
    -> Bool
vectorSubspace { field, addVects } (Scalar scalar) vectorList predicates =
    let
        (Field.Field innerField) =
            field

        (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) =
            innerField

        multiplicationGroup =
            commutativeDivisionRing.multiplication

        multiplicationSemigroup =
            multiplicationGroup.monoid.semigroup

        testzeros =
            List.map (scalarMultiplication field multiplicationGroup.monoid.identity) vectorList

        containszeros =
            closurePassCriteria testzeros

        scaledVectors =
            List.map (scalarMultiplication field scalar) vectorList

        closurePassCriteria =
            List.map (\(Vector vector) -> Vector <| List.map2 Basics.identity predicates vector)
                >> List.all (all ((==) True))

        closureUnderScalarMultiplication =
            closurePassCriteria scaledVectors

        cartesianAddVectors =
            List.Extra.lift2 addVects

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
vectorEqual : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (Vector a)
vectorEqual comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Compare two vectors for equality using a comparator
-}
equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator =
    (vectorEqual comparator).eq


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
readRealVector vectorString =
    Parser.run (parseVector negativeOrPositiveFloat) vectorString


{-| Try to read a string into a Complex Vector
-}
readComplexVector :
    String
    -> Result (List Parser.DeadEnd) (Vector (ComplexNumbers.ComplexNumber Float))
readComplexVector vectorString =
    Parser.run (parseVector ComplexNumbers.parseComplexNumber) vectorString


{-| Find index of a value in a Vector
-}
findIndex : (a -> Bool) -> Vector a -> Maybe Int
findIndex predicate (Vector list) =
    List.Extra.findIndex predicate list
