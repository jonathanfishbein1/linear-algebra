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
    , ColumnVector(..), findPivot, gaussJordan, gaussianReduce, isHermitian, isSymmetric, jordanReduce, scale, solve, subrow, swap
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


{-| Column Vector
-}
type ColumnVector a
    = ColumnVector (Vector.Vector a)


{-| Matrix type
-}
type Matrix a
    = Matrix (List (RowVector a))


type Solution
    = Unique (ColumnVector Float)
    | NoSolution String
    | InfiniteSolutions String


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


isHermitian : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Bool
isHermitian matrix =
    adjoint matrix == matrix


{-| Swap two rows of a matrix
-}
swap : Matrix a -> Int -> Int -> Matrix a
swap (Matrix matrix) rowIndexOne rowIndexTwo =
    case ( matrix, rowIndexOne, rowIndexTwo ) of
        ( xs, 0, 0 ) ->
            Matrix xs

        ( x :: xs, 0, j ) ->
            let
                listValue =
                    Maybe.withDefault (RowVector <| Vector.Vector []) (List.Extra.getAt j (x :: xs))
            in
            Matrix <| listValue :: List.take (j - 1) xs ++ x :: List.drop j xs

        ( xs, i, 0 ) ->
            swap (Matrix xs) 0 i

        ( x :: xs, i, j ) ->
            let
                (Matrix intermediateList) =
                    swap (Matrix xs) (i - 1) (j - 1)
            in
            Matrix <| x :: intermediateList

        ( [], _, _ ) ->
            Matrix matrix


findPivot : Matrix number -> Int -> Maybe Int
findPivot (Matrix matrix) initialRowIndex =
    let
        findRow x =
            Maybe.withDefault (RowVector <| Vector.Vector []) (List.Extra.getAt x matrix)

        findPivotValue nextDiagonalIndex currentRowIndexIteration =
            let
                (RowVector (Vector.Vector currentRowIteration)) =
                    findRow currentRowIndexIteration
            in
            Maybe.withDefault 0 (List.Extra.getAt nextDiagonalIndex currentRowIteration)
    in
    List.head <| List.filter (\x -> findPivotValue initialRowIndex x /= 0) (List.range initialRowIndex (List.length matrix - 1))


scale : Int -> RowVector Float -> RowVector Float
scale rowIndex (RowVector (Vector.Vector rowVector)) =
    case rowVector of
        [] ->
            RowVector <| Vector.Vector []

        xs ->
            let
                elementAtRowIndex =
                    Maybe.withDefault 1 (List.Extra.getAt rowIndex xs)
            in
            RowVector <| Vector.map (\rowElement -> rowElement / elementAtRowIndex) (Vector.Vector xs)


subrow : Int -> RowVector Float -> RowVector Float -> RowVector Float
subrow r (RowVector (Vector.Vector currentRow)) (RowVector (Vector.Vector nextRow)) =
    let
        k =
            Maybe.withDefault 1 (List.Extra.getAt r nextRow)
    in
    List.map2 (\a b -> k * a - b) currentRow nextRow
        |> Vector.Vector
        |> RowVector


reduceRow : Int -> Matrix Float -> Matrix Float
reduceRow rowIndex matrix =
    let
        firstPivot =
            Maybe.withDefault rowIndex (findPivot matrix rowIndex)

        (Matrix swappedListOfRowVectors) =
            swap matrix rowIndex firstPivot

        (Matrix listOfRowVectors) =
            Matrix swappedListOfRowVectors

        row =
            Maybe.withDefault (RowVector <| Vector.Vector []) (List.Extra.getAt rowIndex listOfRowVectors)

        scaledRow =
            scale rowIndex row

        nextRows =
            List.drop (rowIndex + 1) listOfRowVectors
                |> List.map (subrow rowIndex scaledRow)
    in
    List.take rowIndex swappedListOfRowVectors
        ++ [ scaledRow ]
        ++ nextRows
        |> Matrix


gaussianReduce : Matrix Float -> ColumnVector Float -> Matrix Float
gaussianReduce matrix b =
    let
        (Matrix augmentedMatrix) =
            combineMatrixVector matrix b
    in
    List.foldl reduceRow (Matrix augmentedMatrix) (List.range 0 (List.length augmentedMatrix - 1))


jordan : Int -> Matrix Float -> Matrix Float
jordan rowIndex matrix =
    let
        (Matrix listOfRowVectors) =
            matrix

        row =
            Maybe.withDefault (RowVector <| Vector.Vector []) (List.Extra.getAt rowIndex listOfRowVectors)

        prevRows =
            List.take rowIndex listOfRowVectors
                |> List.map
                    (\rowVector ->
                        subrow rowIndex row rowVector
                            |> mapRowVector negate
                    )
    in
    prevRows
        ++ [ row ]
        ++ List.drop (rowIndex + 1) listOfRowVectors
        |> Matrix


jordanReduce : Matrix Float -> Matrix Float
jordanReduce (Matrix matrix) =
    let
        (Matrix thing) =
            List.foldl jordan (Matrix matrix) (List.reverse (List.range 0 (List.length matrix - 1)))
    in
    thing
        |> Matrix


gaussJordan : Matrix Float -> ColumnVector Float -> Matrix Float
gaussJordan matrix b =
    let
        firstReduce =
            Debug.log "firstReduce " (gaussianReduce matrix b)
    in
    firstReduce |> jordanReduce


solve : Matrix Float -> ColumnVector Float -> ColumnVector Float
solve matrix b =
    let
        (Matrix listOfRowVectors) =
            gaussJordan matrix b
    in
    List.foldl (\(RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row) [] listOfRowVectors
        |> Vector.Vector
        |> ColumnVector


mapRowVector : (a -> b) -> RowVector a -> RowVector b
mapRowVector f (RowVector rowVector) =
    Vector.map f rowVector
        |> RowVector


combineMatrixVector : Matrix a -> ColumnVector a -> Matrix a
combineMatrixVector (Matrix listOfRowVectors) (ColumnVector (Vector.Vector list)) =
    List.map2 (\(RowVector (Vector.Vector matrixRow)) vectorElement -> RowVector <| Vector.Vector <| List.append matrixRow [ vectorElement ]) listOfRowVectors list
        |> Matrix
