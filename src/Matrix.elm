module Matrix exposing
    ( Matrix(..)
    , RowVector(..)
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
    , isSymmetric
    )

{-| A module for Matrix


# Types

@docs Matrix
@docs RowVector

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
@docs identityMatrix

-}

import ComplexNumbers
import List.Extra
import Monoid
import Vector


{-| Row Vector
-}
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
multiplyComplexMatrices : Matrix (a -> b) -> Matrix a -> Matrix b
multiplyComplexMatrices matrixOne matrixTwo =
    smartMapMatrix2Generic (transpose matrixTwo) (Vector.Vector []) (Matrix []) matrixOne (transpose matrixTwo)


{-| Matrix Matrix multiplication for a Real Numbered Matrix
-}
multiplyRealMatrices : Matrix (Vector.Vector number) -> Matrix (Vector.Vector number) -> Matrix number
multiplyRealMatrices =
    liftA2Two Vector.realVectorDotProduct


diagonal : Int -> Int -> number
diagonal columnIndex rowIndex =
    if columnIndex == rowIndex then
        1

    else
        0


{-| Create Identity Matrix with n dimension
-}
identityMatrix : Int -> Matrix Int
identityMatrix dimension =
    Matrix (List.Extra.initialize dimension (\columnIndex -> RowVector <| Vector.Vector <| List.Extra.initialize dimension (diagonal columnIndex)))


applyTwo : Matrix (a -> b) -> Matrix a -> Matrix b
applyTwo matrixOne matrixTwo =
    smartMapMatrix2Generic (transpose matrixTwo) (Vector.Vector []) (Matrix []) matrixOne (transpose matrixTwo)


liftA2Two : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
liftA2Two f a b =
    applyTwo (map f a) b


smartMapMatrix2Generic : Matrix a -> Vector.Vector b -> Matrix b -> Matrix (a -> b) -> Matrix a -> Matrix b
smartMapMatrix2Generic (Matrix currentRight) intermediateList (Matrix acc) (Matrix left) (Matrix right) =
    case ( left, currentRight ) of
        ( (RowVector l) :: _, (RowVector r) :: rs ) ->
            smartMapMatrix2Generic (Matrix rs) (Vector.concat (Vector.apply l r) intermediateList) (Matrix acc) (Matrix left) (Matrix right)

        ( _ :: ls, [] ) ->
            smartMapMatrix2Generic (Matrix right) (Vector.Vector []) (Matrix (acc ++ [ RowVector intermediateList ])) (Matrix ls) (Matrix right)

        ( [], _ ) ->
            Matrix acc


{-| Multiply a Vector by a Matrix
-}
multiplyRealVectorRealMatrix : Matrix Int -> Vector.Vector Int -> Matrix number
multiplyRealVectorRealMatrix matrix vector =
    smartMapMatrix2Generic matrix (Vector.Vector []) (Matrix []) (map diagonal matrix) (Matrix <| [ RowVector vector ])


isSymmetric : Matrix a -> Bool
isSymmetric matrix =
    transpose matrix == matrix
