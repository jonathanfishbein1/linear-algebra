module LinearAlgebra exposing
    ( Matrix(..)
    , Vector(..)
    ,  addComplexVectors
       -- , addMatrices

    , addRealVectors
    , equal
    , equalMatrix
    , makeMatrix
    , map
    , matrixConjugate
    , multiply
    , scalarMatrixMultiply
    , sum
    , sumEmpty
    ,  sumEmptyMatrix
       --  , sumMatrices

    , transpose
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


{-| Monoidally add two Vectors together
-}
sum : Monoid.Monoid (Vector number)
sum =
    Monoid.monoid sumEmpty addRealVectors



-- addMatrices : a -> (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
-- addMatrices defaultValue addFunction matrixOne matrixTwo =
--     smartMapMatrix2 defaultValue addFunction matrixOne matrixTwo (Matrix [])


sumEmptyMatrix : Matrix a
sumEmptyMatrix =
    Matrix []


{-| Monoidally add two Matrices together
-}



-- sumMatrices : a -> (a -> a -> a) -> Monoid.Monoid (Matrix a)
-- sumMatrices defaultValue addF =
--     Monoid.monoid sumEmptyMatrix (addMatrices defaultValue addF)


mapMatrix : (a -> b) -> Matrix a -> Matrix b
mapMatrix f (Matrix matrix) =
    Matrix <| List.map (map f) matrix


scalarMatrixMultiply : (a -> b) -> Matrix a -> Matrix b
scalarMatrixMultiply =
    mapMatrix


equalMatrix : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equalMatrix comparator (Matrix matrixOne) (Matrix matrixTwo) =
    List.all ((==) True) <| List.map2 (equal comparator) matrixOne matrixTwo


transpose : Matrix a -> Matrix a
transpose (Matrix matrix) =
    matrix
        |> List.map (\(Vector x) -> x)
        |> List.Extra.transpose
        |> List.map (\x -> Vector x)
        |> Matrix


matrixConjugate : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
matrixConjugate matrix =
    matrix
        |> mapMatrix ComplexNumbers.conjugate
        |> transpose


apply : Vector (a -> b) -> Vector a -> Vector b
apply (Vector fVector) (Vector vector) =
    Vector <| List.map2 (\f x -> f x) fVector vector


liftA2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftA2 f a b =
    apply (map f a) b
