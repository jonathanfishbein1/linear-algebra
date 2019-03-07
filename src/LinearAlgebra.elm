module LinearAlgebra exposing
    ( Matrix(..)
    , Vector(..)
    , add
    , addMatrices
    , equal
    , equalMatrix
    , makeMatrix
    , map
    , matrixConjugate
    , multiply
    , scalarMatrixMultiply
    , scalarMultiply
    , sum
    , sumEmpty
    , sumEmptyMatrix
    , sumMatrices
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


smartMap2 : a -> (a -> a -> a) -> Vector a -> Vector a -> Vector a -> Vector a
smartMap2 defaultValue f (Vector left) (Vector right) (Vector acc) =
    case ( left, right ) of
        ( l :: ls, r :: rs ) ->
            smartMap2 defaultValue f (Vector ls) (Vector rs) (Vector (f l r :: acc))

        ( l :: ls, [] ) ->
            smartMap2 defaultValue f (Vector ls) (Vector []) (Vector (f l defaultValue :: acc))

        ( [], r :: rs ) ->
            smartMap2 defaultValue f (Vector []) (Vector rs) (Vector (f defaultValue r :: acc))

        ( [], [] ) ->
            Vector <| List.reverse acc


smartMapMatrix2 : a -> (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a -> Matrix a
smartMapMatrix2 defaultValue f (Matrix left) (Matrix right) (Matrix acc) =
    case ( left, right ) of
        ( (Vector l) :: ls, (Vector r) :: rs ) ->
            smartMapMatrix2 defaultValue f (Matrix ls) (Matrix rs) (Matrix <| smartMap2 defaultValue f (Vector l) (Vector r) (Vector []) :: acc)

        ( (Vector l) :: ls, [] ) ->
            smartMapMatrix2 defaultValue f (Matrix ls) (Matrix []) (Matrix <| smartMap2 defaultValue f (Vector l) (Vector []) (Vector []) :: acc)

        ( [], (Vector r) :: rs ) ->
            smartMapMatrix2 defaultValue f (Matrix []) (Matrix rs) (Matrix <| smartMap2 defaultValue f (Vector []) (Vector r) (Vector []) :: acc)

        ( [], [] ) ->
            Matrix <| List.reverse acc


add : a -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
add defaultValue addFunction listOone listTwo =
    smartMap2 defaultValue addFunction listOone listTwo (Vector [])


addReal : Vector number -> Vector number -> Vector number
addReal =
    liftA2 (+)


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


realScalarMultiply : (number -> number) -> Vector number -> Vector number
realScalarMultiply =
    map


multiply : (a -> a -> a) -> Vector a -> Vector a -> Vector a
multiply multiplyFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 multiplyFunction vectorOne vectorTwo


equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator (Vector vectorOne) (Vector vectorTwo) =
    List.all ((==) True) <| List.map2 comparator vectorOne vectorTwo


sumEmpty : Vector a
sumEmpty =
    Vector []


{-| Monoidally add two Vectors together
-}
sum : a -> (a -> a -> a) -> Monoid.Monoid (Vector a)
sum defaultValue addF =
    Monoid.monoid sumEmpty (add defaultValue addF)


addMatrices : a -> (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
addMatrices defaultValue addFunction matrixOne matrixTwo =
    smartMapMatrix2 defaultValue addFunction matrixOne matrixTwo (Matrix [])


sumEmptyMatrix : Matrix a
sumEmptyMatrix =
    Matrix []


{-| Monoidally add two Matrices together
-}
sumMatrices : a -> (a -> a -> a) -> Monoid.Monoid (Matrix a)
sumMatrices defaultValue addF =
    Monoid.monoid sumEmptyMatrix (addMatrices defaultValue addF)


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
