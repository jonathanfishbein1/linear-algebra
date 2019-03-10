module Vector exposing
    ( Vector(..)
    , addComplexVectors
    , addRealVectors
    , apply
    , equal
    , map
    , multiplyComplexVectors
    , multiplyRealVectors
    , sumComplex
    , sumEmpty
    , sumReal
    )

import ComplexNumbers
import Monoid


type Vector a
    = Vector (List a)


addComplexVectors : Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number)
addComplexVectors =
    liftA2 ComplexNumbers.add


addRealVectors : Vector number -> Vector number -> Vector number
addRealVectors =
    liftA2 (+)


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


multiplyRealVectors : Vector number -> Vector number -> Vector number
multiplyRealVectors vectorOne vectorTwo =
    liftA2 (*) vectorOne vectorTwo


multiplyComplexVectors : Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number)
multiplyComplexVectors vectorOne vectorTwo =
    liftA2 ComplexNumbers.multiply vectorOne vectorTwo


equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator vectorOne vectorTwo =
    let
        (Vector list) =
            liftA2 comparator vectorOne vectorTwo
    in
    List.all ((==) True) <| list


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


apply : Vector (a -> b) -> Vector a -> Vector b
apply (Vector fVector) (Vector vector) =
    Vector <| List.map2 (\f x -> f x) fVector vector


liftA2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftA2 f a b =
    apply (map f a) b
