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
    , length
    , normalise
    , sum
    , addVectors
    , subtractVectors
    , hadamardMultiplication
    , dotProduct
    , angleBetween
    , cross
    , distance
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

import ComplexNumbers
import Field
import List.Extra
import Parser exposing ((|.), (|=))
import Typeclasses.Classes.Equality
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup


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
    , subtractVects : Vector a -> Vector a -> Vector a
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
    }


{-| Real Numbered Abelian Group
-}
realVectorAbelianGroup : AbelianGroup Float
realVectorAbelianGroup =
    { field = Field.realField
    , addVects = addVectors Field.realField
    , subtractVects = subtractVectors Field.realField
    }


{-| Complex Numbered Abelian Group
-}
complexVectorAbelianGroup : AbelianGroup (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorAbelianGroup =
    { field = ComplexNumbers.complexField
    , addVects = addVectors ComplexNumbers.complexField
    , subtractVects = subtractVectors ComplexNumbers.complexField
    }


{-| Real Numbered Vector Space
-}
realVectorSpace : VectorSpace Float
realVectorSpace =
    { abelianGroup = realVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Field.realField
    }


{-| Complex Numbered Vector Space
-}
complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorSpace =
    { abelianGroup = complexVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication ComplexNumbers.complexField
    }


{-| Real Numbered Inner Product Space
-}
realInnerProductSpace : InnerProductSpace Float
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = dotProduct Field.realField
    }


{-| Complex Numbered Inner Product Space
-}
complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = dotProduct ComplexNumbers.complexField
    }


{-| Zero vector given a Field and dimension
-}
zeros : Field.Field a -> Int -> Vector a
zeros { zero } dim =
    List.repeat dim zero
        |> Vector


{-| Scalar multiplication over a Vector
-}
scalarMultiplication : Field.Field a -> a -> Vector a -> Vector a
scalarMultiplication { multiply } scalar =
    map (multiply scalar)


{-| Calculate the length of a Vector
-}
length : Field.Field a -> Vector a -> a
length field vector =
    dotProduct field vector vector
        |> field.power (1 / 2)


{-| Adjust a vector so that its length is exactly one
-}
normalise : Field.Field a -> Vector a -> Vector a
normalise field v =
    if length field v == field.zero then
        v

    else
        map (field.divide (length field v)) v


{-| Add two Vectors
-}
addVectors : Field.Field a -> Vector a -> Vector a -> Vector a
addVectors { add } =
    map2 add


{-| Subtract Vectors
-}
subtractVectors : Field.Field a -> Vector a -> Vector a -> Vector a
subtractVectors { subtract } =
    map2 subtract


{-| Hadamard Multiplication Vectors
-}
hadamardMultiplication : Field.Field a -> Vector a -> Vector a -> Vector a
hadamardMultiplication { multiply } =
    map2 multiply


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> Vector a -> Vector a -> a
dotProduct field vectorOne vectorTwo =
    hadamardMultiplication field vectorOne vectorTwo
        |> sum field


{-| Calculate the angle between two vectors
-}
angleBetween : Vector Float -> Vector Float -> Float
angleBetween vectorOne vectorTwo =
    let
        dotP =
            dotProduct Field.realField vectorOne vectorTwo

        lengthVectorOne =
            length Field.realField vectorOne

        lengthVectorTwo =
            length Field.realField vectorTwo
    in
    Basics.acos (dotP / (lengthVectorOne * lengthVectorTwo))


{-| Calculate the sum of a Vector
-}
sum : Field.Field a -> Vector a -> a
sum { add, zero } =
    foldl add zero


{-| Calculate distance between two vectors
-}
distance : AbelianGroup a -> Vector a -> Vector a -> a
distance { subtractVects, field } vectorOne vectorTwo =
    subtractVects vectorOne vectorTwo
        |> length field


{-| Take the cross product of two 3D vectors
-}
cross : Field.Field a -> Vector3 a -> Vector3 a -> Vector3 a
cross { subtract, multiply } (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3
        (subtract (multiply y1 z2) (multiply y2 z1))
        (subtract (multiply z1 x2) (multiply z2 x1))
        (subtract (multiply x1 y2) (multiply x2 y1))


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
concat : Typeclasses.Classes.Monoid.Monoid (Vector a)
concat =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend append)
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
        testzeros =
            List.map (scalarMultiplication field field.zero) vectorList

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
printComplexVector : Vector (ComplexNumbers.ComplexNumberCartesian Float) -> String
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
    -> Result (List Parser.DeadEnd) (Vector (ComplexNumbers.ComplexNumberCartesian Float))
readComplexVector vectorString =
    Parser.run (parseVector ComplexNumbers.parseComplexNumber) vectorString


{-| Find index of a value in a Vector
-}
findIndex : (a -> Bool) -> Vector a -> Maybe Int
findIndex predicate (Vector list) =
    List.Extra.findIndex predicate list
