module Matrix exposing
    ( RowVector(..)
    , ColumnVector(..)
    , Matrix(..)
    , Solution(..)
    , Consistancy(..)
    , VectorDimension(..)
    , identityMatrix
    , zeroMatrix
    , zeroSquareMatrix
    , scalarMultiplication
    , transpose
    , conjugate
    , adjoint
    , invert
    , subMatrix
    , nullSpace
    , determinant
    , matrixNorm
    , leftNullSpace
    , addMatrices
    , subtractMatrices
    , multiplyMatrixVector
    , multiplyMatrices
    , dotProduct
    , matrixTensorProduct
    , isSquareMatrix
    , isSymmetric
    , isHermitian
    , isInvertable
    , isUnitary
    , areBasis
    , areLinearlyIndependent
    , doesSetSpanSpace
    , basisOfVectorSpace
    , mDimension
    , nDimension
    , isRightStochastic
    , isLeftStochastic
    , isDoublyStochastic
    , areRowEquivalent
    , matrixEmpty
    , matrixConcatHorizontal
    , matrixConcatVertical
    , map
    , pure
    , apply
    , bind
    , liftA2
    , foldl
    , equal
    , upperTriangle
    , gaussianReduce
    , jordanReduce
    , gaussJordan
    , solve
    , solveMatrix
    , getAt
    , setAt
    , printRealMatrix
    , printComplexMatrix
    , readRealMatrix
    , readComplexMatrix
    )

{-| A module for Matrix


# Types

@docs RowVector
@docs ColumnVector
@docs Matrix
@docs Solution
@docs Consistancy
@docs VectorDimension


# Values

@docs identityMatrix
@docs zeroMatrix
@docs zeroSquareMatrix


# Unitary Operations

@docs scalarMultiplication
@docs transpose
@docs conjugate
@docs adjoint
@docs invert
@docs subMatrix
@docs nullSpace
@docs determinant
@docs matrixNorm
@docs leftNullSpace
@docs getDiagonal


# Binary Operations

@docs addMatrices
@docs subtractMatrices
@docs multiplyMatrixVector
@docs multiplyMatrices
@docs dotProduct
@docs matrixTensorProduct


# Matrix Predicates and Properties

@docs isSquareMatrix
@docs isSymmetric
@docs isHermitian
@docs isInvertable
@docs isUnitary
@docs areBasis
@docs areLinearlyIndependent
@docs doesSetSpanSpace
@docs basisOfVectorSpace
@docs mDimension
@docs nDimension
@docs isRightStochastic
@docs isLeftStochastic
@docs isDoublyStochastic
@docs areRowEquivalent


# Monoid

@docs matrixEmpty
@docs matrixConcatHorizontal
@docs matrixConcatVertical


# Functor, Applicative, Monad, Foldable

@docs map
@docs pure
@docs apply
@docs bind
@docs liftA2
@docs foldl


# Equality

@docs equal


# Matrix Forms

@docs upperTriangle
@docs gaussianReduce
@docs jordanReduce
@docs gaussJordan


# Solving

@docs solve
@docs solveMatrix


# Manipulation

@docs getAt
@docs setAt
@docs printRealMatrix
@docs printComplexMatrix
@docs readRealMatrix
@docs readComplexMatrix

-}

import ComplexNumbers
import Field
import Float.Extra
import Internal.Matrix
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=))
import Typeclasses.Classes.Equality
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup
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
type Consistancy a
    = Consistant (Solution a)
    | Inconsistant String


{-| Type to represent result of Gauss-Jordan reduction if system is consistant
-}
type Solution a
    = UniqueSolution (ColumnVector a)
    | InfiniteSolutions { nullity : Int, rank : Int }


{-| Type to represent vector space such as R, R2, R3
-}
type VectorDimension
    = VectorDimension Int


{-| Type to represent a Abelian Group for Matrix
-}
type alias AbelianGroup a =
    { field : Field.Field a
    , addMatrcs : Matrix a -> Matrix a -> Matrix a
    , subtractMatrcs : Matrix a -> Matrix a -> Matrix a
    }


{-| Create Identity Matrix with n dimension
-}
identityMatrix : Field.Field a -> Int -> Matrix a
identityMatrix field dimension =
    Matrix
        (List.Extra.initialize dimension
            (\columnIndex ->
                List.Extra.initialize
                    dimension
                    (Internal.Matrix.diagonal field columnIndex)
                    |> Vector.Vector
                    |> RowVector
            )
        )


{-| Create Matrix with m x n dimension filled with zeros
-}
zeroMatrix : Field.Field a -> Int -> Int -> Matrix a
zeroMatrix field m n =
    List.Extra.initialize m
        (\_ ->
            Vector.zeroVector field n
                |> RowVector
        )
        |> Matrix


{-| Create square Matrix with n dimension filled with zeros
-}
zeroSquareMatrix : Field.Field a -> Int -> Matrix a
zeroSquareMatrix field dimension =
    zeroMatrix field dimension dimension


{-| Scalar multiplication over a Matrix
-}
scalarMultiplication : Field.Field a -> a -> Matrix a -> Matrix a
scalarMultiplication { multiply } scalar =
    map (multiply scalar)


{-| Transpose a Matrix
-}
transpose : Matrix a -> Matrix a
transpose (Matrix listOfRowVectors) =
    listOfRowVectors
        |> List.map (\(RowVector (Vector.Vector x)) -> x)
        |> List.Extra.transpose
        |> List.map (Vector.Vector >> RowVector)
        |> Matrix


{-| Take the complex conjugate of a Complex Numbered Matrix
-}
conjugate :
    Matrix (ComplexNumbers.ComplexNumberCartesian number)
    -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
conjugate matrix =
    matrix
        |> map ComplexNumbers.conjugate


{-| Perform the adjoint operation on a Complex Numbered Matrix
-}
adjoint :
    Matrix (ComplexNumbers.ComplexNumberCartesian number)
    -> Matrix (ComplexNumbers.ComplexNumberCartesian number)
adjoint matrix =
    matrix
        |> map ComplexNumbers.conjugate
        |> transpose


{-| Calculate the norm of a Matrix
-}
matrixNorm : Vector.InnerProductSpace a -> Matrix a -> Result String a
matrixNorm innerProductSpace matrix =
    dotProduct innerProductSpace matrix matrix
        |> Result.map
            (innerProductSpace.vectorSpace.abelianGroup.field.power (1 / 2))


{-| Try to calculate the determinant
-}
determinant : Vector.VectorSpace a -> Matrix a -> Result String a
determinant vectorSpace matrix =
    let
        upperTriangularForm =
            upperTriangle vectorSpace matrix
    in
    Result.andThen
        (\squareMatrix ->
            getDiagonalProduct vectorSpace.abelianGroup.field squareMatrix
                |> Result.fromMaybe "Index out of range"
        )
        upperTriangularForm


{-| Try to calculate the inverse of a matrix
-}
invert : Vector.VectorSpace a -> Matrix a -> Result String (Matrix a)
invert vectorSpace matrix =
    case isInvertable vectorSpace matrix of
        Ok invertableMatrix ->
            let
                sizeOfMatrix =
                    mDimension invertableMatrix

                augmentedMatrix =
                    appendHorizontal invertableMatrix
                        (identityMatrix vectorSpace.abelianGroup.field sizeOfMatrix)

                reducedRowEchelonForm =
                    gaussJordan vectorSpace augmentedMatrix

                inverse =
                    subMatrix
                        0
                        (mDimension reducedRowEchelonForm)
                        sizeOfMatrix
                        (nDimension reducedRowEchelonForm)
                        reducedRowEchelonForm
            in
            Ok inverse

        Err err ->
            Err err


{-| Calculate the submatrix given a starting and ending row and column index
-}
subMatrix : Int -> Int -> Int -> Int -> Matrix a -> Matrix a
subMatrix startingRowIndex endingRowIndex startingColumnIndex endingColumnIndex (Matrix listOfRowVectors) =
    List.take endingRowIndex listOfRowVectors
        |> List.drop startingRowIndex
        |> List.map
            (\(RowVector (Vector.Vector row)) ->
                List.take endingColumnIndex row
                    |> List.drop startingColumnIndex
                    |> Vector.Vector
                    |> RowVector
            )
        |> Matrix


{-| Calculate the null space of a matrix
-}
nullSpace : Vector.VectorSpace a -> Matrix a -> Consistancy a
nullSpace vectorSpace matrix =
    let
        numberOfRows =
            mDimension matrix

        b =
            List.repeat numberOfRows vectorSpace.abelianGroup.field.zero
                |> Vector.Vector
                |> ColumnVector
    in
    solve vectorSpace matrix b


{-| Calculate the left nullspace of a Matrix
-}
leftNullSpace : Vector.VectorSpace a -> Matrix a -> Consistancy a
leftNullSpace vectorSpace =
    transpose >> nullSpace vectorSpace


getDiagonal : Matrix a -> Maybe (List a)
getDiagonal matrix =
    let
        numberOfRows =
            mDimension matrix

        indices =
            List.Extra.initialize
                numberOfRows
                (\index -> ( index, index ))
    in
    List.foldl
        (\( indexOne, indexTwo ) acc -> getAt ( indexOne, indexTwo ) matrix :: acc)
        []
        indices
        |> Maybe.Extra.combine


getDiagonalProduct : Field.Field a -> Matrix a -> Maybe a
getDiagonalProduct { multiply, one } matrix =
    getDiagonal matrix
        |> Maybe.map
            (List.foldl
                (\elem acc ->
                    multiply
                        elem
                        acc
                )
                one
            )


{-| Add two Matrices together
-}
addMatrices : Field.Field a -> Matrix a -> Matrix a -> Matrix a
addMatrices { add } =
    liftA2 add


{-| Subtract two Matrices
-}
subtractMatrices : Field.Field a -> Matrix a -> Matrix a -> Matrix a
subtractMatrices { subtract } =
    liftA2 subtract


{-| Multiply a Vector by a Matrix
-}
multiplyMatrixVector :
    Vector.InnerProductSpace a
    -> Matrix a
    -> Vector.Vector a
    -> Result String (Vector.Vector a)
multiplyMatrixVector innerProductSpace (Matrix matrix) vector =
    if nDimension (Matrix matrix) == Vector.dimension vector then
        let
            listOfVectors =
                matrix
                    |> List.map (\(RowVector vec) -> vec)
        in
        Internal.Matrix.map2VectorCartesian innerProductSpace listOfVectors [ vector ]
            |> List.foldl
                (\(Vector.Vector elem) acc -> acc ++ elem)
                []
            |> Vector.Vector
            |> Ok

    else
        Err "Matrix has to have the same number of columns as the vector has rows"


{-| Matrix Matrix multiplication
-}
multiplyMatrices :
    Vector.InnerProductSpace a
    -> Matrix a
    -> Matrix a
    -> Result String (Matrix a)
multiplyMatrices innerProductSpace (Matrix matrixOne) matrixTwo =
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
        Internal.Matrix.map2VectorCartesian
            innerProductSpace
            listOfVectorsOne
            listOfVectors
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "first matrix must have same number of columns as the second matrix has rows"


{-| Calculate the dot product of two Matricies
-}
dotProduct : Vector.InnerProductSpace a -> Matrix a -> Matrix a -> Result String a
dotProduct vectorInnerProductSpace matrixOne matrixTwo =
    let
        productMatrix =
            multiplyMatrices vectorInnerProductSpace matrixOne matrixTwo
    in
    case productMatrix of
        Ok pMatrix ->
            if isSquareMatrix pMatrix then
                getDiagonalProduct vectorInnerProductSpace.vectorSpace.abelianGroup.field pMatrix
                    |> Result.fromMaybe "Index out of range"

            else
                Err "Must be Square Matrix"

        Err err ->
            Err err


{-| Calculate the tensor product of two Matricies
-}
matrixTensorProduct : Field.Field a -> Matrix a -> Matrix a -> Matrix a
matrixTensorProduct field matrixOne matrixTwo =
    bind
        matrixOne
        (\matrixOneElement ->
            scalarMultiplication field matrixOneElement matrixTwo
        )


{-| Map over a Matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix listOfRowVectors) =
    Matrix <| List.map (rowVectorMap f) listOfRowVectors


{-| Applicative pure for Matrix
-}
pure : a -> Matrix a
pure a =
    Matrix [ RowVector <| Vector.Vector <| [ a ] ]


{-| Apply for Matrix
-}
apply : Matrix (a -> b) -> Matrix a -> Matrix b
apply (Matrix listOfRowVectorsWithFunctions) (Matrix listOfRowVectors) =
    List.map2
        (\(RowVector fVector) (RowVector xVector) -> RowVector <| Vector.apply fVector xVector)
        listOfRowVectorsWithFunctions
        listOfRowVectors
        |> Matrix


{-| Lift a function to work on Matrix
-}
liftA2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
liftA2 f a b =
    apply (map f a) b


{-| Monad bind for Matrix
-}
bind : Matrix a -> (a -> Matrix b) -> Matrix b
bind (Matrix listOfRowVectors) fMatrix =
    List.concatMap
        (\(RowVector (Vector.Vector listOfElements)) ->
            let
                (Matrix result) =
                    List.concatMap
                        (\element ->
                            let
                                (Matrix resultInner) =
                                    fMatrix element
                            in
                            resultInner
                        )
                        listOfElements
                        |> Matrix
            in
            result
        )
        listOfRowVectors
        |> Matrix


{-| Determine whether a matirx is square
-}
isSquareMatrix : Matrix a -> Bool
isSquareMatrix matrix =
    mDimension matrix == nDimension matrix


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


{-| Determine whether a matirx is unitary
-}
isUnitary : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Bool
isUnitary matrix =
    case invert Vector.complexVectorSpace matrix of
        Ok inverse ->
            equal ComplexNumbers.equal inverse (adjoint matrix)

        Err _ ->
            False


{-| Determine whether a matirx is invertable
-}
isInvertable : Vector.VectorSpace a -> Matrix a -> Result String (Matrix a)
isInvertable vectorSpace matrix =
    case determinant vectorSpace matrix of
        Ok deter ->
            if deter == vectorSpace.abelianGroup.field.zero then
                Err "Determinant is zero matrix is not invertable"

            else
                Ok matrix

        Err msg ->
            Err msg


{-| Predicate if matrix is right stochastic
-}
isRightStochastic : Matrix Float -> Bool
isRightStochastic (Matrix listOfRowVectors) =
    if isSquareMatrix (Matrix listOfRowVectors) then
        List.all
            (\(RowVector (Vector.Vector list)) -> Float.Extra.equalWithin 1.0e-6 (List.sum list) 1)
            listOfRowVectors

    else
        False


{-| Predicate if matrix is left stochastic
-}
isLeftStochastic : Matrix Float -> Bool
isLeftStochastic matrix =
    let
        (Matrix transposedListOfRowVectors) =
            transpose matrix
    in
    if isSquareMatrix (Matrix transposedListOfRowVectors) then
        List.all
            (\(RowVector (Vector.Vector list)) -> Float.Extra.equalWithin 1.0e-6 (List.sum list) 1)
            transposedListOfRowVectors

    else
        False


{-| Predicate if matrix is doubly stochastic
-}
isDoublyStochastic : Matrix Float -> Bool
isDoublyStochastic matrix =
    if isRightStochastic matrix && isLeftStochastic matrix then
        let
            (Matrix listOfRowVectors) =
                matrix
        in
        List.all
            (\(RowVector (Vector.Vector list)) -> List.all ((<=) 0) list)
            listOfRowVectors

    else
        False


{-| Predicate to determine if two matricies are row equivalent
-}
areRowEquivalent : Vector.VectorSpace a -> Matrix a -> Matrix a -> Bool
areRowEquivalent vectorSpace matrixOne matrixTwo =
    gaussJordan vectorSpace matrixOne == gaussJordan vectorSpace matrixTwo


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : Vector.VectorSpace a -> Matrix a -> Result String (Matrix a)
upperTriangle vectorSpace (Matrix matrix) =
    if isSquareMatrix (Matrix matrix) then
        let
            listOfVectors =
                List.map
                    (\(RowVector vector) -> vector)
                    matrix
        in
        List.foldl
            (Internal.Matrix.calculateUpperTriangularFormRectangle vectorSpace)
            listOfVectors
            (List.range 0 (List.length matrix - 1))
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "Must be Square Matrix"


{-| Gaussian Elimination
-}
gaussianReduce : Vector.VectorSpace a -> Matrix a -> Matrix a
gaussianReduce vectorSpace (Matrix matrix) =
    let
        listOfVectors =
            List.map
                (\(RowVector vector) -> vector)
                matrix

        upperTriangularFormRectangle =
            List.foldl
                (Internal.Matrix.calculateUpperTriangularFormRectangle vectorSpace)
                listOfVectors
                (List.range 0 (List.length matrix - 1))

        rowEchelonForm =
            List.indexedMap
                (Internal.Matrix.scale vectorSpace)
                upperTriangularFormRectangle
    in
    rowEchelonForm
        |> List.map RowVector
        |> Matrix


{-| Internal function for Jordan Elimination
-}
jordanReduce : Vector.VectorSpace a -> Matrix a -> Matrix a
jordanReduce vectorSpace (Matrix matrix) =
    let
        listOfVectors =
            List.map
                (\(RowVector vector) -> vector)
                matrix
    in
    List.foldl
        (Internal.Matrix.reduceRowBackwards vectorSpace)
        listOfVectors
        (List.reverse (List.range 0 (List.length matrix - 1)))
        |> List.map RowVector
        |> Matrix


{-| Function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : Vector.VectorSpace a -> Matrix a -> Matrix a
gaussJordan vectorSpace matrix =
    gaussianReduce vectorSpace matrix
        |> jordanReduce vectorSpace


coefficientMatrix : Matrix a -> Matrix a
coefficientMatrix matrix =
    subMatrix 0 (mDimension matrix) 0 (nDimension matrix - 1) matrix


{-| Solve a system of linear equations using Gauss-Jordan elimination
-}
solveMatrix : Vector.VectorSpace a -> Matrix a -> Consistancy a
solveMatrix vectorSpace (Matrix listOfRowVectors) =
    let
        (Matrix listOfRowVectorsRREF) =
            gaussJordan vectorSpace (Matrix listOfRowVectors)

        (Matrix variableSide) =
            coefficientMatrix (Matrix listOfRowVectorsRREF)

        notConstrainedEnough =
            variableSide
                |> List.any
                    (\(RowVector (Vector.Vector row)) ->
                        let
                            countOfOnes =
                                List.Extra.count ((/=) vectorSpace.abelianGroup.field.zero) row
                        in
                        countOfOnes > 1
                    )

        anyAllZeroExceptAugmentedSide =
            listOfRowVectorsRREF
                |> List.any
                    (\(RowVector (Vector.Vector row)) ->
                        List.all
                            ((==) vectorSpace.abelianGroup.field.zero)
                            (List.take (List.length row - 1) row)
                            && Vector.vectorLength vectorSpace.abelianGroup.field (Vector.Vector row)
                            /= vectorSpace.abelianGroup.field.zero
                    )

        solution =
            List.foldl
                (\(RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row)
                []
                listOfRowVectorsRREF
    in
    if anyAllZeroExceptAugmentedSide then
        Inconsistant "No Unique Solution"

    else if notConstrainedEnough then
        let
            rank =
                listOfRowVectorsRREF
                    |> List.Extra.count
                        (\(RowVector vector) ->
                            Vector.vectorLength vectorSpace.abelianGroup.field vector /= vectorSpace.abelianGroup.field.zero
                        )

            nullity =
                nDimension (Matrix listOfRowVectorsRREF) - rank
        in
        InfiniteSolutions { nullity = nullity, rank = rank }
            |> Consistant

    else
        UniqueSolution
            (solution
                |> Vector.Vector
                |> ColumnVector
            )
            |> Consistant


{-| Solve a system of linear equations using Gauss-Jordan elimination with explict column vector of constants
-}
solve : Vector.VectorSpace a -> Matrix a -> ColumnVector a -> Consistancy a
solve vectorSpace matrix (ColumnVector (Vector.Vector constants)) =
    let
        matrixB =
            constants
                |> List.map (List.singleton >> Vector.Vector >> RowVector)
                |> Matrix

        augmentedMatrix =
            matrixConcatHorizontal.semigroup.prepend matrix matrixB
    in
    solveMatrix vectorSpace augmentedMatrix


{-| Predicate to determine if a list of Vectors are linearly independent
-}
areLinearlyIndependent : Vector.VectorSpace a -> List (Vector.Vector a) -> Bool
areLinearlyIndependent vectorSpace listOfVectors =
    let
        listOfRowVectors =
            List.map RowVector listOfVectors

        matrix =
            Matrix listOfRowVectors

        matrixNullSpace =
            nullSpace vectorSpace matrix

        numberOfRows =
            List.length listOfRowVectors
    in
    case matrixNullSpace of
        Consistant (UniqueSolution resultVector) ->
            resultVector == ColumnVector (Vector.zeroVector vectorSpace.abelianGroup.field numberOfRows)

        _ ->
            False


{-| Determine whether list of vectors spans a space
-}
doesSetSpanSpace : Vector.VectorSpace a -> VectorDimension -> List (Vector.Vector a) -> Result String Bool
doesSetSpanSpace vSpace (VectorDimension vectorDimension) vectors =
    if List.length vectors /= vectorDimension then
        Err "Please input same number of vectors as vector space"

    else if not <| List.all (\vector -> Vector.dimension vector == vectorDimension) vectors then
        Err "Please input vectors of equal length as vector space"

    else
        let
            identityRowVectors =
                identityMatrix vSpace.abelianGroup.field vectorDimension

            listOfRowVectorsRREF =
                gaussJordan vSpace (Matrix (List.map RowVector vectors))
        in
        identityRowVectors
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
areBasis : Vector.VectorSpace a -> VectorDimension -> List (Vector.Vector a) -> Bool
areBasis vectorSpace vectorDimension vectors =
    doesSetSpanSpace vectorSpace vectorDimension vectors
        == Ok True
        && areLinearlyIndependent vectorSpace vectors


{-| Determine the basis vectors of a vector space
-}
basisOfVectorSpace :
    Vector.VectorSpace a
    -> VectorDimension
    -> List (Vector.Vector a)
    -> List (Vector.Vector a)
basisOfVectorSpace vectorSpace vectorDimension vectors =
    if areBasis vectorSpace vectorDimension vectors then
        vectors

    else
        let
            (Matrix reducedRowEchelonFormListOfRowVectors) =
                jordanReduce vectorSpace (Matrix (List.map RowVector vectors))
        in
        reducedRowEchelonFormListOfRowVectors
            |> List.map (\(RowVector vector) -> vector)


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
    List.foldl
        (\row accumlator -> rowVectorFoldl foldFunction accumlator row)
        acc
        listOfRowVectors


{-| Monoid empty for Vector
-}
matrixEmpty : Matrix a
matrixEmpty =
    Matrix []


{-| Append Matricies together vertically
-}
appendVertical : Matrix a -> Matrix a -> Matrix a
appendVertical (Matrix listOne) (Matrix listTwo) =
    listOne
        ++ listTwo
        |> Matrix


{-| Monoidally append Matricies together vertically
-}
matrixConcatVertical : Typeclasses.Classes.Monoid.Monoid (Matrix a)
matrixConcatVertical =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend appendVertical)
        matrixEmpty


{-| Append Matricies together horizontally
-}
appendHorizontal : Matrix a -> Matrix a -> Matrix a
appendHorizontal (Matrix listOne) (Matrix listTwo) =
    let
        difference =
            mDimension (Matrix listOne) - mDimension (Matrix listTwo)
    in
    if difference == 0 then
        List.map2
            (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.append rowOne rowTwo)
            listOne
            listTwo
            |> Matrix

    else if difference > 0 then
        List.map2
            (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.append rowOne rowTwo)
            listOne
            (listTwo ++ List.repeat difference (RowVector <| Vector.Vector []))
            |> Matrix

    else
        List.map2
            (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.append rowOne rowTwo)
            (listOne ++ List.repeat (Basics.abs difference) (RowVector <| Vector.Vector []))
            listTwo
            |> Matrix


{-| Monoidally append Matricies together horizontally
-}
matrixConcatHorizontal : Typeclasses.Classes.Monoid.Monoid (Matrix a)
matrixConcatHorizontal =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend appendHorizontal)
        matrixEmpty


{-| Compare two Matrices for equality
-}
equalImplementation : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equalImplementation comparator (Matrix listOfRowVectorsOne) (Matrix listOfRowVectorsTwo) =
    List.all
        ((==) True)
    <|
        List.map2
            (\(RowVector vectorOne) (RowVector vectorTwo) -> Vector.equal comparator vectorOne vectorTwo)
            listOfRowVectorsOne
            listOfRowVectorsTwo


{-| `Equal` type for `Matrix`.
-}
matrixEqual : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (Matrix a)
matrixEqual comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Compare two matricies using comparator
-}
equal : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equal comparator =
    (matrixEqual comparator).eq


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> Matrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (Matrix listOfRowVectors) =
    List.Extra.getAt rowIndex listOfRowVectors
        |> Maybe.andThen (\(RowVector list) -> Vector.getAt columnIndex list)


{-| Set the value in a Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> a -> Matrix a -> Matrix a
setAt ( rowIndex, columnIndex ) element (Matrix listOfRowVectors) =
    List.Extra.getAt rowIndex listOfRowVectors
        |> Maybe.map (\(RowVector list) -> RowVector <| Vector.setAt columnIndex element list)
        |> Maybe.map
            (\newRow ->
                List.Extra.setAt rowIndex newRow listOfRowVectors
            )
        |> Maybe.withDefault listOfRowVectors
        |> Matrix


{-| Print a Real matrix to a string
-}
printRealMatrix : Matrix Float -> String
printRealMatrix (Matrix listOfRowVectors) =
    let
        values =
            List.foldl
                (\(RowVector row) acc -> "RowVector " ++ Vector.printRealVector row ++ " ]" ++ acc)
                ""
                listOfRowVectors
    in
    "Matrix [ " ++ values ++ " ]"


{-| Print a Complex matrix to a string
-}
printComplexMatrix : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> String
printComplexMatrix (Matrix listOfRowVectors) =
    let
        values =
            List.foldl
                (\(RowVector row) acc -> "RowVector " ++ Vector.printComplexVector row ++ " ]" ++ acc)
                ""
                listOfRowVectors
    in
    "Matrix [ " ++ values ++ " ]"


{-| Try to read a string into a Matrix
-}
readRealMatrix : String -> Result (List Parser.DeadEnd) (Matrix Float)
readRealMatrix matrixString =
    Parser.run (parseMatrix Vector.negativeOrPositiveFloat) matrixString


{-| Try to read a string into a Matrix
-}
readComplexMatrix : String -> Result (List Parser.DeadEnd) (Matrix (ComplexNumbers.ComplexNumberCartesian Float))
readComplexMatrix matrixString =
    Parser.run (parseMatrix ComplexNumbers.parseComplexNumber) matrixString


listOfRowVectorParser : Parser.Parser (RowVector a) -> Parser.Parser (List (RowVector a))
listOfRowVectorParser rowVectorParser =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = rowVectorParser
        , trailing = Parser.Forbidden
        }


parseRowVector : Parser.Parser a -> Parser.Parser (RowVector a)
parseRowVector rowVectorElementsParser =
    Parser.succeed RowVector
        |. Parser.keyword "RowVector"
        |. Parser.spaces
        |= Vector.parseVector rowVectorElementsParser


parseMatrix : Parser.Parser a -> Parser.Parser (Matrix a)
parseMatrix matrixElementParser =
    Parser.succeed Matrix
        |. Parser.keyword "Matrix"
        |. Parser.spaces
        |= listOfRowVectorParser (parseRowVector matrixElementParser)


{-| Real numbered Abelian Group for Matrix
-}
realMatrixAbelianGroup : AbelianGroup Float
realMatrixAbelianGroup =
    { field = Field.realField
    , addMatrcs = addMatrices Field.realField
    , subtractMatrcs = subtractMatrices Field.realField
    }
