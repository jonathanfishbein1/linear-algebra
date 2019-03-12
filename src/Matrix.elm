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
    , identityMatrix
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


{-| Matrix type
-}
type Matrix a
    = Matrix (List (Vector.Vector a))


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
map f (Matrix matrix) =
    Matrix <| List.map (Vector.map f) matrix


{-| Compare two Matrices for equality
-}
equal : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equal comparator (Matrix matrixOne) (Matrix matrixTwo) =
    List.all ((==) True) <| List.map2 (Vector.equal comparator) matrixOne matrixTwo


{-| Transpose a Matrix
-}
transpose : Matrix a -> Matrix a
transpose (Matrix matrix) =
    matrix
        |> List.map (\(Vector.Vector x) -> x)
        |> List.Extra.transpose
        |> List.map (\x -> Vector.Vector x)
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
apply (Matrix fMatrix) (Matrix matrix) =
    Matrix <| List.map2 (\fVector xVector -> Vector.apply fVector xVector) fMatrix matrix


{-| Lift a function to work on Matrix
-}
liftA2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
liftA2 f a b =
    apply (map f a) b


{-| Matrix Matrix multiplication for a Complex Numbered Matrix
-}
multiplyComplexMatrices : Matrix (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> Matrix (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> Matrix (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number))
multiplyComplexMatrices matrixOne matrixTwo =
    liftA2 Vector.multiplyComplexVectors matrixOne (transpose matrixTwo)


{-| Matrix Matrix multiplication for a Real Numbered Matrix
-}
multiplyRealMatrices : Matrix number -> Matrix number -> Matrix number
multiplyRealMatrices matrixOne matrixTwo =
    Matrix <| smartMapMatrix2 matrixOne (transpose matrixTwo) (transpose matrixTwo) [] []


diagonal : Int -> Int -> number
diagonal columnIndex rowIndex =
    if columnIndex == rowIndex then
        1

    else
        0


identityMatrix : Int -> Matrix Int
identityMatrix dimension =
    Matrix (List.Extra.initialize dimension (\columnIndex -> Vector.Vector <| List.Extra.initialize dimension (diagonal columnIndex)))



-- multiplyMatr : Matrix number -> Matrix number -> Matrix number
-- matrixFunc (Matrix matrixOne) (Matrix matrixTwo) =
--     Matrix <| smartMapMatrix2 matrixOne matrixTwo []
-- {-| Apply for Matrix
-- -}
-- applyTwo : Matrix number -> Matrix number -> Matrix number
-- applyTwo fMatrix matrix =
--     matrixFunc fMatrix matrix


addSumVectors : Vector.Vector number -> Vector.Vector number -> number
addSumVectors (Vector.Vector vectorOne) (Vector.Vector vectorTwo) =
    List.map2 (*) vectorOne vectorTwo
        |> List.sum


smartMapMatrix2 : Matrix number -> Matrix number -> Matrix number -> List number -> List (Vector.Vector number) -> List (Vector.Vector number)
smartMapMatrix2 (Matrix left) (Matrix right) (Matrix currentRight) intermediateList acc =
    case ( left, currentRight ) of
        ( l :: ls, r :: rs ) ->
            smartMapMatrix2 (Matrix left) (Matrix right) (Matrix rs) (intermediateList ++ [ addSumVectors l r ]) acc

        ( (Vector.Vector l) :: ls, [] ) ->
            smartMapMatrix2 (Matrix ls) (Matrix right) (Matrix right) [] (acc ++ [ Vector.Vector intermediateList ])

        ( [], _ ) ->
            acc
