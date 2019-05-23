module Matrix exposing
    ( Matrix(..)
    , RowVector(..)
    , ColumnVector(..)
    , Solution(..)
    , VectorSpace(..)
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
    , gaussJordan
    , gaussianReduce
    , isHermitian
    , isSymmetric
    , jordanReduce
    , areLinearlyIndependent
    , multiplyRealVectorRealMatrix
    , nullSpace
    , solve
    , areBasis
    , basisOfVectorSpace
    , doesSetSpanSpace
    , mDimension
    , nDimension
    , solveMatrix
    , foldl
    )

{-| A module for Matrix


# Types

@docs Matrix
@docs RowVector
@docs ColumnVector
@docs Solution
@docs VectorSpace

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
@docs areBasis
@docs basisOfVectorSpace
@docs doesSetSpanSpace
@docs mDimension
@docs nDimension
@docs solveMatrix

-}

import ComplexNumbers
import Internal.Matrix
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
    | InfiniteSolutions { nullity : Int, rank : Int }
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
    Matrix <| List.map (rowVectorMap f) listOfRowVectors


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
multiplyComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Result String (Matrix (ComplexNumbers.ComplexNumberCartesian number))
multiplyComplexMatrices (Matrix matrixOne) matrixTwo =
    if nDimension (Matrix matrixOne) == mDimension matrixTwo then
        let
            (Matrix matrixTranspose) =
                transpose matrixTwo

            listOfVectors =
                matrixTranspose
                    |> List.map (\(RowVector vector) -> vector)

            listOfVectorsOne =
                matrixOne
                    |> List.map (\(RowVector vector) -> vector)
        in
        Internal.Matrix.map2VectorCartesianComplex listOfVectors (Vector.Vector []) [] listOfVectorsOne listOfVectors
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "first matrix must have same number of columns as the second matrix has rows"


{-| Matrix Matrix multiplication for a Real Numbered Matrix
-}
multiplyRealMatrices : Matrix number -> Matrix number -> Result String (Matrix number)
multiplyRealMatrices (Matrix matrixOne) matrixTwo =
    if nDimension (Matrix matrixOne) == mDimension matrixTwo then
        let
            (Matrix matrixTranspose) =
                transpose matrixTwo

            listOfVectors =
                matrixTranspose
                    |> List.map (\(RowVector vector) -> vector)

            listOfVectorsOne =
                matrixOne
                    |> List.map (\(RowVector vector) -> vector)
        in
        Internal.Matrix.map2VectorCartesian listOfVectors (Vector.Vector []) [] listOfVectorsOne listOfVectors
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "first matrix must have same number of columns as the second matrix has rows"


{-| Create Identity Matrix with n dimension
-}
identityMatrix : Int -> Matrix Int
identityMatrix dimension =
    Matrix (List.Extra.initialize dimension (\columnIndex -> RowVector <| Vector.Vector <| List.Extra.initialize dimension (Internal.Matrix.diagonal columnIndex)))


{-| Multiply a real Vector by a real Matrix
-}
multiplyRealVectorRealMatrix : Matrix number -> Vector.Vector number -> Matrix number
multiplyRealVectorRealMatrix (Matrix matrix) vector =
    let
        listOfVectors =
            matrix
                |> List.map (\(RowVector vec) -> vec)
    in
    Internal.Matrix.map2VectorCartesian [ vector ] (Vector.Vector []) [] listOfVectors [ vector ]
        |> List.map RowVector
        |> Matrix


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


{-| Internal function Gaussian Elimination
-}
gaussianReduce : Matrix Float -> Matrix Float
gaussianReduce (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix
    in
    List.foldl Internal.Matrix.reduceRow listOfVectors (List.range 0 (List.length matrix - 1))
        |> List.map (\row -> RowVector row)
        |> Matrix


{-| Internal function for Jordan Elimination
-}
jordanReduce : Matrix Float -> Matrix Float
jordanReduce (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix
    in
    List.foldl Internal.Matrix.reduceRowBackwards listOfVectors (List.reverse (List.range 0 (List.length matrix - 1)))
        |> List.map (\row -> RowVector row)
        |> Matrix


{-| Internal function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : Matrix Float -> Matrix Float
gaussJordan matrix =
    gaussianReduce matrix
        |> jordanReduce


{-| Solve a system of linear equations using Gauss-Jordan elimination with explict augmented side column vector
-}
solve : Matrix Float -> ColumnVector Float -> Solution
solve matrix (ColumnVector (Vector.Vector b)) =
    let
        matrixB =
            b
                |> List.map (\x -> [ x ] |> Vector.Vector |> RowVector)
                |> Matrix

        augmentedMatrix =
            matrixConcat matrix matrixB
    in
    solveMatrix augmentedMatrix


variablePortion : Matrix Float -> Matrix Float
variablePortion (Matrix listOfRowVectors) =
    List.map (\(RowVector (Vector.Vector row)) -> RowVector <| Vector.Vector <| List.take (List.length row - 1) row) listOfRowVectors
        |> Matrix


{-| Solve a system of linear equations using Gauss-Jordan elimination
-}
solveMatrix : Matrix Float -> Solution
solveMatrix (Matrix listOfRowVectors) =
    let
        (Matrix listOfRowVectorsRREF) =
            gaussJordan (Matrix listOfRowVectors)

        (Matrix variableSide) =
            variablePortion (Matrix listOfRowVectorsRREF)

        notConstrainedEnough =
            variableSide
                |> List.any
                    (\(RowVector (Vector.Vector row)) ->
                        let
                            countOfOnes =
                                List.Extra.count ((/=) 0) row
                        in
                        countOfOnes > 1
                    )

        anyAllZeroExceptAugmentedSide =
            listOfRowVectorsRREF
                |> List.any (\(RowVector (Vector.Vector row)) -> List.all ((==) 0) (List.take (List.length row - 1) row) && Vector.realVectorLength (Vector.Vector row) /= 0)

        solution =
            List.foldl (\(RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row) [] listOfRowVectorsRREF
    in
    if anyAllZeroExceptAugmentedSide then
        NoUniqueSolution "No Unique Solution"

    else if notConstrainedEnough then
        let
            rank =
                listOfRowVectorsRREF
                    |> List.Extra.count (\(RowVector vector) -> Vector.realVectorLength vector /= 0)

            nullity =
                nDimension (Matrix listOfRowVectorsRREF) - rank
        in
        InfiniteSolutions { nullity = nullity, rank = rank }

    else
        UniqueSolution
            (solution
                |> Vector.Vector
                |> ColumnVector
            )


{-| Calculate the null space of a matrix
-}
nullSpace : Matrix Float -> Solution
nullSpace matrix =
    let
        numberOfRows =
            mDimension matrix

        b =
            List.Extra.initialize numberOfRows (\_ -> 0)
                |> Vector.Vector
                |> ColumnVector
    in
    solve matrix b


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


{-| Determine whether list of vectors spans a space
-}
doesSetSpanSpace : VectorSpace -> List (RowVector Float) -> Result String Bool
doesSetSpanSpace (VectorSpace vectorSpace) rowVectors =
    if List.length rowVectors /= vectorSpace then
        Err "Please input same number of vectors as vector space"

    else if not <| List.all (\(RowVector row) -> Vector.dimension row == vectorSpace) rowVectors then
        Err "Please input vectors of equal length as vector space"

    else
        let
            identityRowVectors =
                identityMatrix vectorSpace

            floatMatrix =
                identityRowVectors
                    |> map toFloat

            listOfRowVectorsRREF =
                gaussJordan (Matrix rowVectors)
        in
        floatMatrix
            == listOfRowVectorsRREF
            |> Ok


{-| Number of columns in Matrix
-}
nDimension : Matrix a -> Int
nDimension (Matrix listOfRowVectors) =
    case listOfRowVectors of
        [] ->
            0

        (RowVector x) :: _ ->
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
    if doesSetSpanSpace vectorSpace rowVectors == Ok True && areLinearlyIndependent rowVectors then
        True

    else
        False


matrixConcat : Matrix a -> Matrix a -> Matrix a
matrixConcat (Matrix matrixOne) (Matrix matrixTwo) =
    List.map2 (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.concat rowOne rowTwo) matrixOne matrixTwo
        |> Matrix


{-| Determine the basis vectors of a vector space
-}
basisOfVectorSpace : VectorSpace -> List (RowVector Float) -> List (RowVector Float)
basisOfVectorSpace vectorSpace rowVectors =
    if areBasis vectorSpace rowVectors then
        rowVectors

    else
        let
            (Matrix reducedRowEchelonFormListOfRowVectors) =
                jordanReduce (Matrix rowVectors)
        in
        reducedRowEchelonFormListOfRowVectors


rowVectorMap : (a -> b) -> RowVector a -> RowVector b
rowVectorMap f (RowVector vector) =
    Vector.map f vector
        |> RowVector


{-| Left fold over a RowVector
-}
rowVectorFoldl : (a -> b -> b) -> b -> RowVector a -> b
rowVectorFoldl foldFunction acc (RowVector vector) =
    Vector.foldl foldFunction acc vector


{-| Left fold over a Matrix
-}
foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl foldFunction acc (Matrix listOfRowVectors) =
    List.foldl (\row accumlator -> rowVectorFoldl foldFunction accumlator row) acc listOfRowVectors
