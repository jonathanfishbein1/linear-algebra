module Vector exposing
    ( Vector(..)
    , Vector3(..)
    , Scalar(..)
    , map
    , equal
    , apply
    , liftA2
    , foldl
    , concat
    , cross
    , distance
    , normalise
    , vector3ToVector
    , dimension
    , append
    , concatEmpty
    , pure
    , bind
    , findIndex
    , getAt
    , parseVector
    , print
    , read
    , setAt
    , InnerProductSpace, VectorSpace, addVectors, complexInnerProductSpace, complexVectorAbelianGroup, complexVectorSpace, realInnerProductSpace, realVectorAbelianGroup, realVectorSpace, scalarMultiplication, subtractVectors, vectorDotProduct, vectorLength, vectorSubspace, vectorTensorProduct
    )

{-| A module for Vectors


# Types

@docs Vector
@docs Vector3
@docs Scalar

@docs addComplexVectors
@docs addRealVectors
@docs map
@docs multiplyRealVectors
@docs multiplyComplexVectors
@docs equal
@docs apply
@docs liftA2
@docs foldl
@docs realVectorDotProduct
@docs complexVectorDotProduct
@docs concat
@docs complexVectorLength
@docs cross
@docs distance
@docs normalise
@docs vectorLength Field.realField
@docs subtractRealVectors
@docs subtractComplexVectors
@docs vector3ToVector
@docs dimension
@docs realVectorSubspace
@docs complexVectorSubspace
@docs append
@docs concatEmpty
@docs pure
@docs bind
@docs findIndex
@docs getAt
@docs parseVector
@docs print
@docs read
@docs setAt

-}

import ComplexNumbers
import Field
import Float.Extra
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


type alias AbelianGroup a =
    { field : Field.Field a
    , addVects : Vector a -> Vector a -> Vector a
    , subtractVects : Vector a -> Vector a -> Vector a
    }


type alias VectorSpace a =
    { abelianGroup : AbelianGroup a
    , vectorScalarMultiplication : a -> Vector a -> Vector a
    }


type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : Vector a -> Vector a -> a
    }


addVectors : Field.Field a -> Vector a -> Vector a -> Vector a
addVectors { add } =
    liftA2 add


{-| Map over a vector
-}
map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


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


vectorDotProduct : Field.Field a -> Vector a -> Vector a -> a
vectorDotProduct { zero, add, multiply } vectorOne vectorTwo =
    liftA2 multiply vectorOne vectorTwo
        |> foldl add zero


vectorLength : Field.Field a -> Vector a -> a
vectorLength { power, add, zero } =
    foldl (\x acc -> add (power 2 x) acc) zero
        >> power (1 / 2)


{-| Subtract Real Vectors together
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


{-| Convert a Vector3 type to a Vector typeZ
-}
vector3ToVector : Vector3 number -> Vector number
vector3ToVector (Vector3 x y z) =
    Vector [ x, y, z ]


{-| Adjust a vector so that its length is exactly one
-}
normalise : Vector Float -> Vector Float
normalise v =
    if Float.Extra.equalWithin 0.000000001 (vectorLength Field.realField v) 0.0 then
        v

    else
        map ((/) (vectorLength Field.realField v)) v


{-| Count of number of elements in a vector
-}
dimension : Vector a -> Int
dimension (Vector list) =
    List.length list


vectorSubspace : AbelianGroup a -> Scalar a -> List (Vector a) -> List (a -> Bool) -> Bool
vectorSubspace { field, addVects } (Scalar scalar) vectorList predicates =
    let
        testZeroVector =
            List.map (map (field.multiply field.zero)) vectorList

        containsZeroVector =
            closurePassCriteria testZeroVector

        scaledVectors =
            List.map (map (field.multiply scalar)) vectorList

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


{-| Print a Vector as a string
-}
print : Vector Float -> String
print (Vector list) =
    let
        values =
            List.map String.fromFloat list
                |> String.join ", "
    in
    "Vector [" ++ values ++ "]"


listParser : Parser.Parser (List Float)
listParser =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = myNumber
        , trailing = Parser.Forbidden
        }


{-| Parse a Vector
-}
parseVector : Parser.Parser (Vector Float)
parseVector =
    Parser.succeed Vector
        |. Parser.keyword "Vector"
        |. Parser.spaces
        |= listParser


{-| Try to read a string into a Vector
-}
read : String -> Result (List Parser.DeadEnd) (Vector Float)
read vectorString =
    Parser.run parseVector vectorString


float : Parser.Parser Float
float =
    Parser.number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }


myNumber : Parser.Parser Float
myNumber =
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


realVectorAbelianGroup : AbelianGroup Float
realVectorAbelianGroup =
    { field = Field.realField
    , addVects = addVectors Field.realField
    , subtractVects = subtractVectors Field.realField
    }


complexVectorAbelianGroup : AbelianGroup (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorAbelianGroup =
    { field = ComplexNumbers.complexField
    , addVects = addVectors ComplexNumbers.complexField
    , subtractVects = subtractVectors ComplexNumbers.complexField
    }


realVectorSpace : VectorSpace Float
realVectorSpace =
    { abelianGroup = realVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Field.realField
    }


complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorSpace =
    { abelianGroup = complexVectorAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication ComplexNumbers.complexField
    }


realInnerProductSpace : InnerProductSpace Float
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = vectorDotProduct Field.realField
    }


complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = vectorDotProduct ComplexNumbers.complexField
    }


vectorTensorProduct : Field.Field a -> Vector a -> Vector a -> Vector a
vectorTensorProduct { multiply } vectorOne vectorTwo =
    bind
        vectorOne
        (\vectorOneElement ->
            map (multiply vectorOneElement) vectorTwo
        )
