module Matrix exposing
    (  Matrix(..)
       -- , addMatrices

    , equalMatrix
    , makeMatrix
    , matrixConjugate
    , scalarMatrixMultiply
    ,  sumEmptyMatrix
       --  , sumMatrices

    , transpose
    )

import ComplexNumbers
import List.Extra
import Monoid
import Vector


type Matrix a
    = Matrix (List (Vector.Vector a))


allSameBy : (a -> comparable) -> List a -> Bool
allSameBy f list =
    List.length (List.Extra.uniqueBy f list) == 1


makeMatrix : List (Vector.Vector a) -> Result String (Matrix a)
makeMatrix listOfVectors =
    if allSameBy (\(Vector.Vector x) -> List.length x) listOfVectors then
        Ok <|
            Matrix listOfVectors

    else
        Err "list has differnt inner list length: Malformed input"



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
    Matrix <| List.map (Vector.map f) matrix


scalarMatrixMultiply : (a -> b) -> Matrix a -> Matrix b
scalarMatrixMultiply =
    mapMatrix


equalMatrix : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equalMatrix comparator (Matrix matrixOne) (Matrix matrixTwo) =
    List.all ((==) True) <| List.map2 (Vector.equal comparator) matrixOne matrixTwo


transpose : Matrix a -> Matrix a
transpose (Matrix matrix) =
    matrix
        |> List.map (\(Vector.Vector x) -> x)
        |> List.Extra.transpose
        |> List.map (\x -> Vector.Vector x)
        |> Matrix


matrixConjugate : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
matrixConjugate matrix =
    matrix
        |> mapMatrix ComplexNumbers.conjugate
        |> transpose
