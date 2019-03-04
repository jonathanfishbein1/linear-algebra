module LinearAlgebra exposing
    ( Matrix(..)
    , Vector(..)
    , add
    , addMatrices
    , equal
    , equalMatrix
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


smartMap2 : a -> (a -> a -> a) -> List a -> List a -> List a -> List a
smartMap2 defaultValue f left right acc =
    case ( left, right ) of
        ( l :: ls, r :: rs ) ->
            smartMap2 defaultValue f ls rs (f l r :: acc)

        ( l :: ls, [] ) ->
            smartMap2 defaultValue f ls [] (f l defaultValue :: acc)

        ( [], r :: rs ) ->
            smartMap2 defaultValue f [] rs (f defaultValue r :: acc)

        ( [], [] ) ->
            List.reverse acc


smartMapMatrix2 : a -> (a -> a -> a) -> List (Vector a) -> List (Vector a) -> List (Vector a) -> List (Vector a)
smartMapMatrix2 defaultValue f left right acc =
    case ( left, right ) of
        ( (Vector l) :: ls, (Vector r) :: rs ) ->
            smartMapMatrix2 defaultValue f ls rs ((Vector <| smartMap2 defaultValue f l r []) :: acc)

        ( (Vector l) :: ls, [] ) ->
            smartMapMatrix2 defaultValue f ls [] ((Vector <| smartMap2 defaultValue f l [] []) :: acc)

        ( [], (Vector r) :: rs ) ->
            smartMapMatrix2 defaultValue f [] rs ((Vector <| smartMap2 defaultValue f [] r []) :: acc)

        ( [], [] ) ->
            List.reverse acc


add : a -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
add defaultValue addFunction (Vector listOone) (Vector listTwo) =
    Vector <| smartMap2 defaultValue addFunction listOone listTwo []


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


scalarMultiply : (a -> b) -> Vector a -> Vector b
scalarMultiply =
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
addMatrices defaultValue addFunction (Matrix matrixOne) (Matrix matrixTwo) =
    Matrix <| smartMapMatrix2 defaultValue addFunction matrixOne matrixTwo []


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
