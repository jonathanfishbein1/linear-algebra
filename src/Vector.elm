module Vector exposing
    ( Vector(..)
    , Vector3(..)
    , Scalar(..)
    , AbelianGroup
    , VectorSpace
    , InnerProductSpace
    , addVectors
    , map
    , equal
    , apply
    , liftA2
    , foldl
    , vectorDotProduct
    , concat
    , vectorLength
    , cross
    , distance
    , normalise
    , subtractVectors
    , vector3ToVector
    , dimension
    , vectorSubspace
    , append
    , concatEmpty
    , pure
    , bind
    , findIndex
    , getAt
    , parseVector
    , printRealVector
    , printComplexVector
    , readRealVector
    , readComplexVector
    , setAt
    , vectorTensorProduct
    , scalarMultiplication
    , realVectorSpace
    , realVectorAbelianGroup
    , realInnerProductSpace
    , negativeOrPositiveFloat
    , complexVectorSpace
    , complexVectorAbelianGroup
    , complexInnerProductSpace
    )

{-| A module for Vectors


# Types

@docs Vector
@docs Vector3
@docs Scalar
@docs AbelianGroup
@docs VectorSpace
@docs InnerProductSpace

@docs addVectors
@docs map
@docs equal
@docs apply
@docs liftA2
@docs foldl
@docs vectorDotProduct
@docs concat
@docs vectorLength
@docs cross
@docs distance
@docs normalise
@docs subtractVectors
@docs vector3ToVector
@docs dimension
@docs vectorSubspace
@docs append
@docs concatEmpty
@docs pure
@docs bind
@docs findIndex
@docs getAt
@docs parseVector
@docs printRealVector
@docs printComplexVector
@docs readRealVector
@docs readComplexVector
@docs setAt
@docs vectorTensorProduct
@docs scalarMultiplication
@docs realVectorSpace
@docs realVectorAbelianGroup
@docs realInnerProductSpace
@docs negativeOrPositiveFloat
@docs complexVectorSpace
@docs complexVectorAbelianGroup
@docs complexInnerProductSpace

-}

import ComplexNumbers
import Field
import List.Extra
import Parser exposing ((|.), (|=))
import Typeclasses.Classes.Equality
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup


{-| Vector type
-}
type Vector a
    = Vector (List a)


{-| 3 Dimensional Vector type
-}
type Vector3 a
    = Vector3 a a a


{-| Type to represent a scalar value
-}
type Scalar a
    = Scalar a


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


{-| Add two Vectors
-}
addVectors : Field.Field a -> Vector a -> Vector a -> Vector a
addVectors { add } =
    liftA2 add


{-| Map over a vector
-}
map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


{-| Scalar multiplication over a Vector
-}
scalarMultiplication : Field.Field a -> a -> Vector a -> Vector a
scalarMultiplication { multiply } scalar =
    map (multiply scalar)


{-| Compare two Vectors for equality
-}
equalImplementation : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equalImplementation comparator vectorOne vectorTwo =
    let
        (Vector list) =
            liftA2 comparator vectorOne vectorTwo
    in
    List.all ((==) True) <| list


{-| Monoid empty for Vector
-}
concatEmpty : Vector a
concatEmpty =
    Vector []


{-| Monoidally append Vectors together
-}
concat : Typeclasses.Classes.Monoid.Monoid (Vector a)
concat =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend append) concatEmpty


{-| Append Vectors together
-}
append : Vector a -> Vector a -> Vector a
append (Vector listOne) (Vector listTwo) =
    listOne
        ++ listTwo
        |> Vector


{-| Place a value in minimal Vector context
-}
pure : a -> Vector a
pure a =
    Vector [ a ]


{-| Apply for Vector
-}
apply : Vector (a -> b) -> Vector a -> Vector b
apply (Vector fVector) (Vector vector) =
    Vector <| List.map2 (\f x -> f x) fVector vector


{-| Lift a binary function to work with Vectors
-}
liftA2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftA2 f a b =
    apply (map f a) b


{-| Left fold over a Vector
-}
foldl : (a -> b -> b) -> b -> Vector a -> b
foldl foldFunction acc (Vector list) =
    List.foldl foldFunction acc list


{-| Calculate the dot product of two Vectors
-}
vectorDotProduct : Field.Field a -> Vector a -> Vector a -> a
vectorDotProduct { zero, add, multiply } vectorOne vectorTwo =
    liftA2 multiply vectorOne vectorTwo
        |> foldl add zero


{-| Calculate the length of a Vector
-}
vectorLength : Field.Field a -> Vector a -> a
vectorLength { power, add, zero } =
    foldl (\x acc -> add (power 2 x) acc) zero
        >> power (1 / 2)


{-| Subtract Vectors
-}
subtractVectors : Field.Field a -> Vector a -> Vector a -> Vector a
subtractVectors { subtract } =
    liftA2 subtract


{-| Calculate distance between two vectors
-}
distance : AbelianGroup a -> Vector a -> Vector a -> a
distance { subtractVects, field } vectorOne vectorTwo =
    subtractVects vectorOne vectorTwo
        |> vectorLength field


{-| Take the cross product of two 3D vectors
-}
cross : Field.Field a -> Vector3 a -> Vector3 a -> Vector3 a
cross { subtract, multiply } (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3
        (subtract (multiply y1 z2) (multiply y2 z1))
        (subtract (multiply z1 x2) (multiply z2 x1))
        (subtract (multiply x1 y2) (multiply x2 y1))


{-| Convert a Vector3 type to a Vector type
-}
vector3ToVector : Vector3 a -> Vector a
vector3ToVector (Vector3 x y z) =
    Vector [ x, y, z ]


{-| Adjust a vector so that its length is exactly one
-}
normalise : Field.Field a -> Vector a -> Vector a
normalise field v =
    if vectorLength field v == field.zero then
        v

    else
        map (field.divide (vectorLength field v)) v


{-| Count of number of elements in a vector
-}
dimension : Vector a -> Int
dimension (Vector list) =
    List.length list


{-| Determine whether a list of Vectors makes a Subspace
-}
vectorSubspace : AbelianGroup a -> Scalar a -> List (Vector a) -> List (a -> Bool) -> Bool
vectorSubspace { field, addVects } (Scalar scalar) vectorList predicates =
    let
        testZeroVector =
            List.map (scalarMultiplication field field.zero) vectorList

        containsZeroVector =
            closurePassCriteria testZeroVector

        scaledVectors =
            List.map (scalarMultiplication field scalar) vectorList

        closurePassCriteria =
            List.map (\(Vector vector) -> Vector <| List.map2 (\predicate x -> predicate x) predicates vector)
                >> List.all (\(Vector vector) -> List.all ((==) True) vector)

        closureUnderScalarMultiplication =
            closurePassCriteria scaledVectors

        cartesianAddVectors =
            List.Extra.lift2 addVects

        additionOfVectors =
            cartesianAddVectors vectorList vectorList

        closureUnderAddition =
            closurePassCriteria additionOfVectors
    in
    containsZeroVector && closureUnderScalarMultiplication && closureUnderAddition


{-| bind for Vector
-}
bind : Vector a -> (a -> Vector b) -> Vector b
bind (Vector list) fVector =
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
printRealVector (Vector list) =
    let
        values =
            List.map String.fromFloat list
                |> String.join ", "
    in
    "Vector [" ++ values ++ "]"


{-| Print a Complex Vector as a string
-}
printComplexVector : Vector (ComplexNumbers.ComplexNumberCartesian Float) -> String
printComplexVector (Vector list) =
    let
        values =
            List.map ComplexNumbers.print list
                |> String.join ", "
    in
    "Vector [" ++ values ++ "]"


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
readComplexVector : String -> Result (List Parser.DeadEnd) (Vector (ComplexNumbers.ComplexNumberCartesian Float))
readComplexVector vectorString =
    Parser.run (parseVector ComplexNumbers.parseComplexNumber) vectorString


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


{-| Find index of a value in a Vector
-}
findIndex : (a -> Bool) -> Vector a -> Maybe Int
findIndex predicate (Vector list) =
    List.Extra.findIndex predicate list


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
    , innerProduct = vectorDotProduct Field.realField
    }


{-| Complex Numbered Inner Product Space
-}
complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = vectorDotProduct ComplexNumbers.complexField
    }


{-| Calculate the tensor product of two vectors
-}
vectorTensorProduct : Field.Field a -> Vector a -> Vector a -> Vector a
vectorTensorProduct field vectorOne vectorTwo =
    bind
        vectorOne
        (\vectorOneElement ->
            scalarMultiplication field vectorOneElement vectorTwo
        )
