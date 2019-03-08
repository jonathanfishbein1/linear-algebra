module Matrix exposing
    ( Matrix(..)
    ,  addComplexMatrices
       --  , sumMatrices

    , addRealMatrices
    , equalMatrix
    , matrixConjugate
    , scalarMatrixMultiply
    , sumEmptyMatrix
    , transpose
    )

import ComplexNumbers
import List.Extra
import Monoid
import Vector


type Matrix a
    = Matrix (List (Vector.Vector a))


addRealMatrices : Matrix number -> Matrix number -> Matrix number
addRealMatrices =
    liftA2 (+)


addComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
addComplexMatrices =
    liftA2 ComplexNumbers.add


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


apply : Matrix (a -> b) -> Matrix a -> Matrix b
apply (Matrix fMatrix) (Matrix matrix) =
    Matrix <| List.map2 (\fVector xVector -> Vector.apply fVector xVector) fMatrix matrix


liftA2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
liftA2 f a b =
    apply (mapMatrix f a) b
