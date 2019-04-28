module Vector exposing
    ( Vector(..)
    , Vector3(..)
    , addComplexVectors
    , addRealVectors
    , map
    , multiplyRealVectors
    , multiplyComplexVectors
    , equal
    , sumEmpty
    , sumReal
    , sumComplex
    , apply
    , liftA2
    , foldl
    , realVectorDotProduct
    , complexVectorDotProduct
    , concat
    , complexVectorLength
    , cross
    , distance, normalise, realVectorLength, subtractRealVectors, vector3ToVector
    )

{-| A module for Vectors


# Types

@docs Vector
@docs Vector3

@docs addComplexVectors
@docs addRealVectors
@docs map
@docs multiplyRealVectors
@docs multiplyComplexVectors
@docs equal
@docs sumEmpty
@docs sumReal
@docs sumComplex
@docs apply
@docs liftA2
@docs foldl
@docs realVectorDotProduct
@docs complexVectorDotProduct
@docs concat
@docs complexVectorLength
@docs cross

-}

import ComplexNumbers
import Float.Extra
import Monoid


{-| Vector type
-}
type Vector a
    = Vector (List a)


{-| 3 Dimensional Vector type
-}
type Vector3 a
    = Vector3 a a a


{-| Add Complex Vectors together
-}
addComplexVectors : Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number)
addComplexVectors =
    liftA2 ComplexNumbers.add


{-| Add Real Vectors together
-}
addRealVectors : Vector number -> Vector number -> Vector number
addRealVectors =
    liftA2 (+)


{-| Map over a vector
-}
map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


{-| Multiply two Real Vectors together
-}
multiplyRealVectors : Vector number -> Vector number -> Vector number
multiplyRealVectors vectorOne vectorTwo =
    liftA2 (*) vectorOne vectorTwo


{-| Multiply two Complex Vectors together
-}
multiplyComplexVectors : Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number)
multiplyComplexVectors vectorOne vectorTwo =
    liftA2 ComplexNumbers.multiply vectorOne vectorTwo


{-| Compare two Vectors for equality
-}
equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator vectorOne vectorTwo =
    let
        (Vector list) =
            liftA2 comparator vectorOne vectorTwo
    in
    List.all ((==) True) <| list


{-| Monoid empty for Vector
-}
sumEmpty : List a -> Vector a
sumEmpty =
    Vector


{-| Monoidally add Real Vectors together
-}
sumReal : List number -> Monoid.Monoid (Vector number)
sumReal a =
    Monoid.monoid (sumEmpty a) addRealVectors


{-| Monoidally add Complex Vectors together
-}
sumComplex : List (ComplexNumbers.ComplexNumberCartesian number) -> Monoid.Monoid (Vector (ComplexNumbers.ComplexNumberCartesian number))
sumComplex a =
    Monoid.monoid (sumEmpty a) addComplexVectors


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


{-| Dot product on two Complex Numbered Vectors
-}
complexVectorDotProduct : Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number) -> ComplexNumbers.ComplexNumberCartesian number
complexVectorDotProduct vectorOne vectorTwo =
    liftA2 ComplexNumbers.multiply vectorOne vectorTwo
        |> foldl ComplexNumbers.add ComplexNumbers.zero


{-| Dot product on two Real Numbered Vectors
-}
realVectorDotProduct : Vector number -> Vector number -> number
realVectorDotProduct vectorOne vectorTwo =
    liftA2 (*) vectorOne vectorTwo
        |> foldl (+) 0


{-| Concat two Vectors together
-}
concat : Vector a -> Vector a -> Vector a
concat (Vector listOne) (Vector listTwo) =
    Vector <| listOne ++ listTwo


{-| Calculate length of a real vector
-}
realVectorLength : Vector Float -> Float
realVectorLength =
    foldl (\x acc -> x ^ 2 + acc) 0
        >> Basics.sqrt


{-| Calculate length of a complex vector
-}
complexVectorLength : Vector (ComplexNumbers.ComplexNumberCartesian Float) -> ComplexNumbers.ComplexNumberCartesian Float
complexVectorLength complexNumbers =
    let
        complexNumbersPolar =
            map ComplexNumbers.convertFromCartesianToPolar complexNumbers
    in
    foldl (\x acc -> ComplexNumbers.add (ComplexNumbers.power 2 x |> ComplexNumbers.convertFromPolarToCartesian) acc) ComplexNumbers.zero complexNumbersPolar


{-| Subtract Real Vectors together
-}
subtractRealVectors : Vector number -> Vector number -> Vector number
subtractRealVectors =
    liftA2 (-)


distance : Vector Float -> Vector Float -> Float
distance vectorOne vectorTwo =
    subtractRealVectors vectorOne vectorTwo
        |> realVectorLength


{-| Take the cross product of two 3D vectors
-}
cross : Vector3 number -> Vector3 number -> Vector3 number
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3
        (y1 * z2 - y2 * z1)
        (z1 * x2 - z2 * x1)
        (x1 * y2 - x2 * y1)


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
