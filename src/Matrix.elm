module Matrix exposing
    ( Matrix(..)
    , addRealMatrices
    , addComplexMatrices
    , sumRealMatrices
    , sumComplexMatrices
    , map
    , equal
    , transpose
    , conjugate
    , adjoint
    , apply
    , liftA2
    , multiplyComplexMatrices
    , multiplyRealMatrices
    , RowVector(..), identityMatrix
    )

{-| A module for Matrix


# Types

@docs Matrix

@docs addRealMatrices
@docs addComplexMatrices
@docs sumRealMatrices
@docs sumComplexMatrices
@docs map
@docs equal
@docs transpose
@docs conjugate
@docs adjoint
@docs apply
@docs liftA2
@docs multiplyComplexMatrices
@docs multiplyRealMatrices

-}

import ComplexNumbers
import List.Extra
import Monoid
import Vector


type RowVector a
    = RowVector (Vector.Vector a)


{-| Matrix type
-}
type Matrix a
    = Matrix (List (RowVector a))


{-| Add two Real Matrices together
-}
addRealMatrices : Matrix number -> Matrix number -> Matrix number
addRealMatrices =
    liftA2 (+)


{-| Add two Complex Matrices together
-}
addComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
addComplexMatrices =
    liftA2 ComplexNumbers.add


{-| Monoidally add two Real numbered Matrices together
-}
sumRealMatrices : Matrix number -> Monoid.Monoid (Matrix number)
sumRealMatrices sumEmptyMatrix =
    Monoid.monoid sumEmptyMatrix addRealMatrices


{-| Monoidally add two Complex numbered Matrices together
-}
sumComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Monoid.Monoid (Matrix (ComplexNumbers.ComplexNumberCartesian number))
sumComplexMatrices sumEmptyMatrix =
    Monoid.monoid sumEmptyMatrix addComplexMatrices


{-| Map over a Matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix listOfRowVectors) =
    Matrix <| List.map (\(RowVector vector) -> RowVector <| Vector.map f vector) listOfRowVectors


{-| Compare two Matrices for equality
-}
equal : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equal comparator (Matrix listOfRowVectorsOne) (Matrix listOfRowVectorsTwo) =
    List.all ((==) True) <| List.map2 (\(RowVector vectorOne) (RowVector vectorTwo) -> Vector.equal comparator vectorOne vectorTwo) listOfRowVectorsOne listOfRowVectorsTwo


{-| Transpose a Matrix
-}
transpose : Matrix a -> Matrix a
transpose (Matrix listOfRowVectors) =
    listOfRowVectors
        |> List.map (\(RowVector (Vector.Vector x)) -> x)
        |> List.Extra.transpose
        |> List.map (\x -> RowVector <| Vector.Vector x)
        |> Matrix


{-| Take the complex conjugate of a Complex Numbered Matrix
-}
conjugate : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
conjugate matrix =
    matrix
        |> map ComplexNumbers.conjugate


{-| Perform the adjoint operation on a Complex Numbered Matrix
-}
adjoint : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
adjoint matrix =
    matrix
        |> map ComplexNumbers.conjugate
        |> transpose


{-| Apply for Matrix
-}
apply : Matrix (a -> b) -> Matrix a -> Matrix b
apply (Matrix listOfRowVectorsWithFunctions) (Matrix listOfRowVectors) =
    Matrix <| List.map2 (\(RowVector fVector) (RowVector xVector) -> RowVector <| Vector.apply fVector xVector) listOfRowVectorsWithFunctions listOfRowVectors


{-| Lift a function to work on Matrix
-}
liftA2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
liftA2 f a b =
    apply (map f a) b


{-| Matrix Matrix multiplication for a Complex Numbered Matrix
-}
multiplyComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
multiplyComplexMatrices matrixOne matrixTwo =
    smartMapComplexMatrix2 matrixOne (transpose matrixTwo) (transpose matrixTwo) [] (Matrix [])


{-| Matrix Matrix multiplication for a Real Numbered Matrix
-}
multiplyRealMatrices : Matrix number -> Matrix number -> Matrix number
multiplyRealMatrices matrixOne matrixTwo =
    smartMapMatrix2 matrixOne (transpose matrixTwo) (transpose matrixTwo) [] (Matrix [])


diagonal : Int -> Int -> number
diagonal columnIndex rowIndex =
    if columnIndex == rowIndex then
        1

    else
        0


identityMatrix : Int -> Matrix Int
identityMatrix dimension =
    Matrix (List.Extra.initialize dimension (\columnIndex -> RowVector <| Vector.Vector <| List.Extra.initialize dimension (diagonal columnIndex)))



-- multiplyMatr : Matrix number -> Matrix number -> Matrix number
-- matrixFunc (Matrix matrixOne) (Matrix matrixTwo) =
--     Matrix <| smartMapMatrix2 matrixOne matrixTwo []
-- {-| Apply for Matrix
-- -}
-- applyTwo : Matrix number -> Matrix number -> Matrix number
-- applyTwo fMatrix matrix =
--     matrixFunc fMatrix matrix


addSumVectors : RowVector number -> RowVector number -> number
addSumVectors (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.liftA2 (*) vectorOne vectorTwo
        |> (\(Vector.Vector x) -> x)
        |> List.sum


smartMapMatrix2 : Matrix number -> Matrix number -> Matrix number -> List number -> Matrix number -> Matrix number
smartMapMatrix2 (Matrix left) (Matrix right) (Matrix currentRight) intermediateList (Matrix acc) =
    case ( left, currentRight ) of
        ( l :: _, r :: rs ) ->
            smartMapMatrix2 (Matrix left) (Matrix right) (Matrix rs) (intermediateList ++ [ addSumVectors l r ]) (Matrix acc)

        ( _ :: ls, [] ) ->
            smartMapMatrix2 (Matrix ls) (Matrix right) (Matrix right) [] (Matrix (acc ++ [ RowVector <| Vector.Vector intermediateList ]))

        ( [], _ ) ->
            Matrix acc


addSumComplexVectors : RowVector (ComplexNumbers.ComplexNumberCartesian number) -> RowVector (ComplexNumbers.ComplexNumberCartesian number) -> ComplexNumbers.ComplexNumberCartesian number
addSumComplexVectors (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.liftA2 ComplexNumbers.multiply vectorOne vectorTwo
        |> Vector.foldl ComplexNumbers.add ComplexNumbers.zero


smartMapComplexMatrix2 : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> List (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
smartMapComplexMatrix2 (Matrix left) (Matrix right) (Matrix currentRight) intermediateList (Matrix acc) =
    case ( left, currentRight ) of
        ( l :: _, r :: rs ) ->
            smartMapComplexMatrix2 (Matrix left) (Matrix right) (Matrix rs) (intermediateList ++ [ addSumComplexVectors l r ]) (Matrix acc)

        ( _ :: ls, [] ) ->
            smartMapComplexMatrix2 (Matrix ls) (Matrix right) (Matrix right) [] (Matrix (acc ++ [ RowVector <| Vector.Vector intermediateList ]))

        ( [], _ ) ->
            Matrix acc
