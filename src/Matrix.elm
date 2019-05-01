module Matrix exposing
    ( Matrix(..)
    , RowVector(..)
    , ColumnVector(..)
    , Solution(..)
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
    , findPivot
    , gaussJordan
    , gaussianReduce
    , isHermitian
    , isSymmetric
    , jordanReduce
    , areLinearlyIndependent
    , multiplyRealVectorRealMatrix
    , nullSpace
    , scale
    , solve
    , subrow
    , swap
    , VectorSpace(..), areBasis, doesSetSpanSpace
    )

{-| A module for Matrix


# Types

@docs Matrix
@docs RowVector
@docs ColumnVector
@docs Solution

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
@docs findPivot
@docs gaussJordan
@docs gaussianReduce
@docs isHermitian
@docs isSymmetric
@docs jordanReduce
@docs areLinearlyIndependent
@docs multiplyRealVectorRealMatrix
@docs nullSpace
@docs scale
@docs solve
@docs subrow
@docs swap

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


{-| Type to represent result of Gauss-Jordan reduction
-}
type Solution
    = UniqueSolution (ColumnVector Float)
    | NoUniqueSolution String


{-| Type to represent vector space such as R, R2, R3
-}
type VectorSpace
    = VectorSpace Int


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
    map2VectorCartesianComplex (transpose matrixTwo) (RowVector <| Vector.Vector []) (Matrix []) matrixOne (transpose matrixTwo)


{-| Matrix Matrix multiplication for a Real Numbered Matrix
-}
multiplyRealMatrices : Matrix number -> Matrix number -> Matrix number
multiplyRealMatrices matrixOne matrixTwo =
    map2VectorCartesian (transpose matrixTwo) (RowVector <| Vector.Vector []) (Matrix []) matrixOne (transpose matrixTwo)


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


map2VectorCartesian : Matrix number -> RowVector number -> Matrix number -> Matrix number -> Matrix number -> Matrix number
map2VectorCartesian (Matrix right) (RowVector (Vector.Vector intermediateList)) (Matrix acc) (Matrix left) (Matrix currentRight) =
    case ( left, currentRight ) of
        ( (RowVector l) :: _, (RowVector r) :: rs ) ->
            map2VectorCartesian (Matrix right) (RowVector <| Vector.Vector (intermediateList ++ [ Vector.realVectorDotProduct l r ])) (Matrix acc) (Matrix left) (Matrix rs)

        ( _ :: ls, [] ) ->
            map2VectorCartesian (Matrix right) (RowVector (Vector.Vector [])) (Matrix (acc ++ [ RowVector <| Vector.Vector <| intermediateList ])) (Matrix ls) (Matrix right)

        ( [], _ ) ->
            Matrix acc


map2VectorCartesianComplex : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> RowVector (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
map2VectorCartesianComplex (Matrix right) (RowVector (Vector.Vector intermediateList)) (Matrix acc) (Matrix left) (Matrix currentRight) =
    case ( left, currentRight ) of
        ( (RowVector l) :: _, (RowVector r) :: rs ) ->
            map2VectorCartesianComplex (Matrix right) (RowVector <| Vector.Vector (intermediateList ++ [ Vector.complexVectorDotProduct l r ])) (Matrix acc) (Matrix left) (Matrix rs)

        ( _ :: ls, [] ) ->
            map2VectorCartesianComplex (Matrix right) (RowVector (Vector.Vector [])) (Matrix (acc ++ [ RowVector <| Vector.Vector <| intermediateList ])) (Matrix ls) (Matrix right)

        ( [], _ ) ->
            Matrix acc


{-| Multiply a real Vector by a real Matrix
-}
multiplyRealVectorRealMatrix : Matrix number -> Vector.Vector number -> Matrix number
multiplyRealVectorRealMatrix matrix vector =
    map2VectorCartesian (Matrix <| [ RowVector vector ]) (RowVector <| Vector.Vector []) (Matrix []) matrix (Matrix <| [ RowVector vector ])


{-| Predicate to determine if Matrix is symmetric
-}
isSymmetric : Matrix a -> Bool
isSymmetric matrix =
    transpose matrix == matrix


{-| Predicate to determine if Matrix is Hermitian
-}
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


{-| Internal function for finding pivot entry in Gaussian elimination
-}
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


{-| Internal function for scalling rows by pivot entry
-}
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


{-| Internal function for subtracting rows from each other
-}
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


{-| Internal function Gaussian Elimination
-}
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


{-| Internal function for Jordan Elimination
-}
jordanReduce : Matrix Float -> Matrix Float
jordanReduce (Matrix matrix) =
    let
        (Matrix thing) =
            List.foldl jordan (Matrix matrix) (List.reverse (List.range 0 (List.length matrix - 1)))
    in
    thing
        |> Matrix


{-| Internal function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : Matrix Float -> ColumnVector Float -> Matrix Float
gaussJordan matrix b =
    gaussianReduce matrix b
        |> jordanReduce


{-| Solve a system of linear equations using Gauss-Jordan elimination
-}
solve : Matrix Float -> ColumnVector Float -> Solution
solve matrix b =
    let
        (Matrix listOfRowVectors) =
            gaussJordan matrix b

        solution =
            List.foldl (\(RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row) [] listOfRowVectors
    in
    if List.all isNaN solution then
        NoUniqueSolution "No Unique Solution"

    else
        UniqueSolution
            (solution
                |> Vector.Vector
                |> ColumnVector
            )


mapRowVector : (a -> b) -> RowVector a -> RowVector b
mapRowVector f (RowVector rowVector) =
    Vector.map f rowVector
        |> RowVector


combineMatrixVector : Matrix a -> ColumnVector a -> Matrix a
combineMatrixVector (Matrix listOfRowVectors) (ColumnVector (Vector.Vector list)) =
    List.map2 (\(RowVector (Vector.Vector matrixRow)) vectorElement -> RowVector <| Vector.Vector <| List.append matrixRow [ vectorElement ]) listOfRowVectors list
        |> Matrix


{-| Calculate the null space of a matrix
-}
nullSpace : Matrix Float -> Solution
nullSpace (Matrix listOfRowVectors) =
    let
        numberOfRows =
            List.length listOfRowVectors

        b =
            List.Extra.initialize numberOfRows (\_ -> 0)
                |> Vector.Vector
                |> ColumnVector
    in
    solve (Matrix listOfRowVectors) b


{-| Predicate to determine if a list of Vectors are linearly independent
-}
areLinearlyIndependent : List (RowVector Float) -> Bool
areLinearlyIndependent listOfRowVectors =
    let
        matrix =
            Matrix listOfRowVectors

        matrixNullSpace =
            nullSpace matrix

        numberOfRows =
            List.length listOfRowVectors

        zeroVector =
            List.Extra.initialize numberOfRows (\_ -> 0)
                |> Vector.Vector
                |> ColumnVector
    in
    case matrixNullSpace of
        UniqueSolution resultVector ->
            if resultVector == zeroVector then
                True

            else
                False

        _ ->
            False


{-| Convert a RowVector into a ColumnVector
-}
rowVectorTranspose : RowVector a -> ColumnVector a
rowVectorTranspose (RowVector vector) =
    ColumnVector vector


{-| Determine whether list of vectors spans a space
-}
doesSetSpanSpace : VectorSpace -> List (RowVector Float) -> Bool
doesSetSpanSpace (VectorSpace vectorSpace) rowVectors =
    let
        (Matrix transposedListOfRowVectors) =
            rowVectors
                |> Matrix
                |> transpose

        (Matrix identityRowVectors) =
            identityMatrix vectorSpace

        identityColumnVectors =
            List.map rowVectorTranspose identityRowVectors
                |> List.map (\(ColumnVector vector) -> ColumnVector <| Vector.map toFloat vector)

        matrix =
            List.foldl (\elem acc -> combineMatrixVector acc elem) (Matrix transposedListOfRowVectors) identityColumnVectors

        mD =
            mDimension matrix

        result =
            solve matrix (ColumnVector <| Vector.Vector <| List.Extra.initialize mD (\_ -> 0))
    in
    case result of
        UniqueSolution _ ->
            True

        _ ->
            False


{-| Number of columns in Matrix
-}
nDimension : Matrix a -> Int
nDimension (Matrix listOfRowVectors) =
    case listOfRowVectors of
        [] ->
            0

        (RowVector x) :: xs ->
            Vector.dimension x


{-| Number of rows in Matrix
-}
mDimension : Matrix a -> Int
mDimension (Matrix listOfRowVectors) =
    List.length listOfRowVectors


{-| Determine whether list of vectors are a basis for a space
-}
areBasis : VectorSpace -> List (RowVector Float) -> Bool
areBasis vectorSpace rowVectors =
    if doesSetSpanSpace vectorSpace rowVectors && areLinearlyIndependent rowVectors then
        True

    else
        False
