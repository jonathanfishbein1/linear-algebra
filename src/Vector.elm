module Vector exposing
    ( Vector(..)
    , addComplexVectors
    , addRealVectors
    , equal
    , map
    , multiply
    , sumComplex
    , sumEmpty
    , sumReal
    )

import ComplexNumbers
import List.Extra
import Monoid


type Vector a
    = Vector (List a)


type Matrix a
    = Matrix (List (Vector a))


allSameBy : (a -> comparable) -> List a -> Bool
allSameBy f list =
    List.length (List.Extra.uniqueBy f list) == 1


makeMatrix : List (Vector a) -> Result String (Matrix a)
makeMatrix listOfVectors =
    if allSameBy (\(Vector x) -> List.length x) listOfVectors then
        Ok <|
            Matrix listOfVectors

    else
        Err "list has differnt inner list length: Malformed input"


addComplexVectors : Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number) -> Vector (ComplexNumbers.ComplexNumberCartesian number)
addComplexVectors =
    liftA2 ComplexNumbers.add


addRealVectors : Vector number -> Vector number -> Vector number
addRealVectors =
    liftA2 (+)


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


multiply : (a -> a -> a) -> Vector a -> Vector a -> Vector a
multiply multiplyFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 multiplyFunction vectorOne vectorTwo


equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator vectorOne vectorTwo =
    let
        (Vector list) =
            liftA2 comparator vectorOne vectorTwo
    in
    List.all ((==) True) <| list


sumEmpty : Vector a
sumEmpty =
    Vector []


{-| Monoidally add Real Vectors together
-}
sumReal : Monoid.Monoid (Vector number)
sumReal =
    Monoid.monoid sumEmpty addRealVectors


{-| Monoidally add Complex Vectors together
-}
sumComplex : Monoid.Monoid (Vector (ComplexNumbers.ComplexNumberCartesian number))
sumComplex =
    Monoid.monoid sumEmpty addComplexVectors


applyVector : Vector (a -> b) -> Vector a -> Vector b
applyVector (Vector fVector) (Vector vector) =
    Vector <| List.map2 (\f x -> f x) fVector vector


liftA2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftA2 f a b =
    applyVector (map f a) b
