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
    , complexVectorLength
    , cross
    , distance
    , normalise
    , realVectorLength
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
    , InnerProductSpace, VectorSpace, addVectors, complexInnerProductSpace, complexVectorAbelianGroup, complexVectorSpace, realInnerProductSpace, realVectorAbelianGroup, realVectorSpace, subtractVectors, vectorDotProduct, vectorSubspace, vectorTensorProduct
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
@docs realVectorLength
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
import Float.Extra
import Internal.Field
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
    { field : Internal.Field.Field a
    , addVects : Vector a -> Vector a -> Vector a
    , subtractVectors : Vector a -> Vector a -> Vector a
    }


type alias VectorSpace a =
    { abelianGroup : AbelianGroup a
    }


type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : Vector a -> Vector a -> a
    }


addVectors : Internal.Field.Field a -> Vector a -> Vector a -> Vector a
addVectors { add } =
    liftA2 add


{-| Map over a vector
-}
map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


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


vectorDotProduct : Internal.Field.Field a -> Vector a -> Vector a -> a
vectorDotProduct { zero, add, multiply } vectorOne vectorTwo =
    liftA2 multiply vectorOne vectorTwo
        |> foldl add zero


{-| Calculate length of a real vector
-}
realVectorLength : Vector Float -> Float
realVectorLength =
    foldl (\x acc -> x ^ 2 + acc) 0
        >> Basics.sqrt


{-| Calculate length of a complex vector
-}
complexVectorLength : Vector (ComplexNumbers.ComplexNumberCartesian Float) -> ComplexNumbers.ComplexNumberCartesian Float
complexVectorLength complexNumberVector =
    foldl (\x acc -> ComplexNumbers.add (ComplexNumbers.power 2 x) acc) ComplexNumbers.zero complexNumberVector


{-| Subtract Real Vectors together
-}
subtractVectors : Internal.Field.Field a -> Vector a -> Vector a -> Vector a
subtractVectors { subtract } =
    liftA2 subtract


{-| Calculate distance between two vectors
-}
distance : Vector Float -> Vector Float -> Float
distance vectorOne vectorTwo =
    subtractVectors Internal.Field.realField vectorOne vectorTwo
        |> realVectorLength


{-| Take the cross product of two 3D vectors
-}
cross : Vector3 number -> Vector3 number -> Vector3 number
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3
        (y1 * z2 - y2 * z1)
        (z1 * x2 - z2 * x1)
        (x1 * y2 - x2 * y1)


{-| Convert a Vector3 type to a Vector typeZ
-}
vector3ToVector : Vector3 number -> Vector number
vector3ToVector (Vector3 x y z) =
    Vector [ x, y, z ]


{-| Adjust a vector so that its length is exactly one
-}
normalise : Vector Float -> Vector Float
normalise v =
    if Float.Extra.equalWithin 0.000000001 (realVectorLength v) 0.0 then
        v

    else
        map ((/) (realVectorLength v)) v


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
    { field = Internal.Field.realField
    , addVects = addVectors Internal.Field.realField
    , subtractVectors = subtractVectors Internal.Field.realField
    }


complexVectorAbelianGroup : AbelianGroup (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorAbelianGroup =
    { field = Internal.Field.complexField
    , addVects = addVectors Internal.Field.complexField
    , subtractVectors = subtractVectors Internal.Field.complexField
    }


realVectorSpace : VectorSpace Float
realVectorSpace =
    { abelianGroup = realVectorAbelianGroup
    }


complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorSpace =
    { abelianGroup = complexVectorAbelianGroup
    }


realInnerProductSpace : InnerProductSpace Float
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = vectorDotProduct Internal.Field.realField
    }


complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = vectorDotProduct Internal.Field.complexField
    }


vectorTensorProduct : Internal.Field.Field a -> Vector a -> Vector a -> Vector a
vectorTensorProduct { multiply } vectorOne vectorTwo =
    bind
        vectorOne
        (\vectorOneElement ->
            map (multiply vectorOneElement) vectorTwo
        )
