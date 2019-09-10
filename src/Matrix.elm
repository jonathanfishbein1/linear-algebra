module Matrix exposing
    ( Matrix(..)
    , RowVector(..)
    , ColumnVector(..)
    , Solution(..)
    , VectorDimension(..)
    , map
    , equal
    , transpose
    , conjugate
    , adjoint
    , apply
    , liftA2
    , identityMatrix
    , gaussJordan
    , gaussianReduce
    , isHermitian
    , isSymmetric
    , jordanReduce
    , areLinearlyIndependent
    , nullSpace
    , solve
    , areBasis
    , basisOfVectorSpace
    , doesSetSpanSpace
    , mDimension
    , nDimension
    , solveMatrix
    , foldl
    , matrixConcatHorizontal
    , matrixConcatVertical
    , matrixEmpty
    , pure
    , bind
    , determinant
    , getAt
    , print
    , read
    , setAt
    , upperTriangle
    , invert
    , isUnitary
    , subMatrix
    , addMatrices, isInvertable, isSquareMatrix, multiplyMatrices, multiplyVectorMatrix, sumMatrices
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
@docs solve
@docs areBasis
@docs basisOfVectorSpace
@docs doesSetSpanSpace
@docs mDimension
@docs nDimension
@docs solveMatrix
@docs foldl
@docs matrixConcatHorizontal
@docs matrixConcatVertical
@docs matrixEmpty
@docs pure
@docs bind
@docs determinant
@docs getAt
@docs print
@docs read
@docs setAt
@docs upperTriangle
@docs determinantComplex
@docs gaussianReduceComplex
@docs invert
@docs invertComplex
@docs isUnitary
@docs jordanReduceComplex
@docs upperTriangleComplex
@docs subMatrix

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
type Solution a
    = UniqueSolution (ColumnVector a)
    | InfiniteSolutions { nullity : Int, rank : Int }
    | NoUniqueSolution String


{-| Type to represent vector space such as R, R2, R3
-}
type VectorDimension
    = VectorDimension Int


type alias AbelianGroup a =
    { field : Field.Field a
    , addMatrcs : Matrix a -> Matrix a -> Matrix a
    , subtractMatrcs : Matrix a -> Matrix a -> Matrix a
    }


{-| Add two Real Matrices together
-}
addMatrices : Field.Field a -> Matrix a -> Matrix a -> Matrix a
addMatrices { add } =
    liftA2 add


{-| Add two Real Matrices together
-}
subtractMatrices : Field.Field a -> Matrix a -> Matrix a -> Matrix a
subtractMatrices { subtract } =
    liftA2 subtract


{-| Monoidally add two Real numbered Matrices together
-}
sumMatrices : AbelianGroup a -> Matrix a -> Typeclasses.Classes.Monoid.Monoid (Matrix a)
sumMatrices { addMatrcs } sumEmptyMatrix =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend addMatrcs) sumEmptyMatrix


{-| Map over a Matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix listOfRowVectors) =
    Matrix <| List.map (rowVectorMap f) listOfRowVectors


{-| Compare two Matrices for equality
-}
equalImplementation : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equalImplementation comparator (Matrix listOfRowVectorsOne) (Matrix listOfRowVectorsTwo) =
    List.all ((==) True) <| List.map2 (\(RowVector vectorOne) (RowVector vectorTwo) -> Vector.equal comparator vectorOne vectorTwo) listOfRowVectorsOne listOfRowVectorsTwo


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


{-| Matrix Matrix multiplication for a Real Numbered Matrix
-}
multiplyMatrices : Vector.InnerProductSpace a -> Matrix a -> Matrix a -> Result String (Matrix a)
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
        Internal.Matrix.map2VectorCartesian innerProductSpace listOfVectorsOne listOfVectors
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "first matrix must have same number of columns as the second matrix has rows"


{-| Create Identity Matrix with n dimension
-}
identityMatrix : Field.Field a -> Int -> Matrix a
identityMatrix field dimension =
    Matrix (List.Extra.initialize dimension (\columnIndex -> RowVector <| Vector.Vector <| List.Extra.initialize dimension (Internal.Matrix.diagonal field columnIndex)))


{-| Multiply a real Vector by a real Matrix
-}
multiplyVectorMatrix : Vector.InnerProductSpace a -> Matrix a -> Vector.Vector a -> Vector.Vector a
multiplyVectorMatrix innerProductSpace (Matrix matrix) vector =
    let
        listOfVectors =
            matrix
                |> List.map (\(RowVector vec) -> vec)
    in
    Internal.Matrix.map2VectorCartesian innerProductSpace listOfVectors [ vector ]
        |> List.foldl (\(Vector.Vector elem) acc -> acc ++ elem) []
        |> Vector.Vector


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


{-| Gaussian Elimination
-}
gaussianReduce : Vector.VectorSpace a -> Matrix a -> Matrix a
gaussianReduce vectorSpace (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix

        upperTriangularFormRectangle =
            List.foldl (Internal.Matrix.calculateUpperTriangularFormRectangle vectorSpace) listOfVectors (List.range 0 (List.length matrix - 1))

        rowEchelonForm =
            List.indexedMap
                (Internal.Matrix.scale vectorSpace)
                upperTriangularFormRectangle
    in
    rowEchelonForm
        |> List.map RowVector
        |> Matrix


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : Vector.VectorSpace a -> Matrix a -> Result String (Matrix a)
upperTriangle vectorSpace (Matrix matrix) =
    if isSquareMatrix (Matrix matrix) then
        let
            listOfVectors =
                List.map (\(RowVector vector) -> vector) matrix
        in
        List.foldl (Internal.Matrix.calculateUpperTriangularFormRectangle vectorSpace) listOfVectors (List.range 0 (List.length matrix - 1))
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "Must be Square Matrix"


{-| Internal function for Jordan Elimination
-}
jordanReduce : Vector.VectorSpace a -> Matrix a -> Matrix a
jordanReduce vectorSpace (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix
    in
    List.foldl (Internal.Matrix.reduceRowBackwards vectorSpace) listOfVectors (List.reverse (List.range 0 (List.length matrix - 1)))
        |> List.map RowVector
        |> Matrix


{-| Function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : Vector.VectorSpace a -> Matrix a -> Matrix a
gaussJordan vectorSpace matrix =
    gaussianReduce vectorSpace matrix
        |> jordanReduce vectorSpace


{-| Solve a system of linear equations using Gauss-Jordan elimination with explict augmented side column vector
-}
solve : Vector.VectorSpace a -> Matrix a -> ColumnVector a -> Solution a
solve vectorSpace matrix (ColumnVector (Vector.Vector b)) =
    let
        matrixB =
            b
                |> List.map (List.singleton >> Vector.Vector >> RowVector)
                |> Matrix

        augmentedMatrix =
            matrixConcatHorizontal.semigroup.prepend matrix matrixB
    in
    solveMatrix vectorSpace augmentedMatrix


variablePortion : Matrix a -> Matrix a
variablePortion matrix =
    subMatrix 0 (mDimension matrix) 0 (nDimension matrix - 1) matrix


{-| Solve a system of linear equations using Gauss-Jordan elimination
-}
solveMatrix : Vector.VectorSpace a -> Matrix a -> Solution a
solveMatrix vectorSpace (Matrix listOfRowVectors) =
    let
        (Matrix listOfRowVectorsRREF) =
            gaussJordan vectorSpace (Matrix listOfRowVectors)

        (Matrix variableSide) =
            variablePortion (Matrix listOfRowVectorsRREF)

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
                |> List.any (\(RowVector (Vector.Vector row)) -> List.all ((==) vectorSpace.abelianGroup.field.zero) (List.take (List.length row - 1) row) && Vector.vectorLength vectorSpace.abelianGroup.field (Vector.Vector row) /= vectorSpace.abelianGroup.field.zero)

        solution =
            List.foldl (\(RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row) [] listOfRowVectorsRREF
    in
    if anyAllZeroExceptAugmentedSide then
        NoUniqueSolution "No Unique Solution"

    else if notConstrainedEnough then
        let
            rank =
                listOfRowVectorsRREF
                    |> List.Extra.count (\(RowVector vector) -> Vector.vectorLength vectorSpace.abelianGroup.field vector /= vectorSpace.abelianGroup.field.zero)

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
nullSpace : Vector.VectorSpace a -> Matrix a -> Solution a
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

        zeroVector =
            List.repeat numberOfRows vectorSpace.abelianGroup.field.zero
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

            floatMatrix =
                identityRowVectors

            listOfRowVectorsRREF =
                gaussJordan vSpace (Matrix (List.map RowVector vectors))
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
areBasis : Vector.VectorSpace a -> VectorDimension -> List (Vector.Vector a) -> Bool
areBasis vectorSpace vectorDimension vectors =
    if doesSetSpanSpace vectorSpace vectorDimension vectors == Ok True && areLinearlyIndependent vectorSpace vectors then
        True

    else
        False


{-| Determine the basis vectors of a vector space
-}
basisOfVectorSpace : Vector.VectorSpace a -> VectorDimension -> List (Vector.Vector a) -> List (Vector.Vector a)
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
    List.foldl (\row accumlator -> rowVectorFoldl foldFunction accumlator row) acc listOfRowVectors


{-| Append Matricies together vertically
-}
appendVertical : Matrix a -> Matrix a -> Matrix a
appendVertical (Matrix listOne) (Matrix listTwo) =
    listOne
        ++ listTwo
        |> Matrix


{-| Monoid empty for Vector
-}
matrixEmpty : Matrix a
matrixEmpty =
    Matrix []


{-| Monoidally append Matricies together vertically
-}
matrixConcatVertical : Typeclasses.Classes.Monoid.Monoid (Matrix a)
matrixConcatVertical =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend appendVertical) matrixEmpty


{-| Append Matricies together horizontally
-}
appendHorizontal : Matrix a -> Matrix a -> Matrix a
appendHorizontal (Matrix listOne) (Matrix listTwo) =
    let
        difference =
            mDimension (Matrix listOne) - mDimension (Matrix listTwo)
    in
    if difference == 0 then
        List.map2 (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.append rowOne rowTwo) listOne listTwo
            |> Matrix

    else if difference > 0 then
        List.map2 (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.append rowOne rowTwo) listOne (listTwo ++ List.repeat difference (RowVector <| Vector.Vector []))
            |> Matrix

    else
        List.map2 (\(RowVector rowOne) (RowVector rowTwo) -> RowVector <| Vector.append rowOne rowTwo) (listOne ++ List.repeat (Basics.abs difference) (RowVector <| Vector.Vector [])) listTwo
            |> Matrix


{-| Monoidally append Matricies together horizontally
-}
matrixConcatHorizontal : Typeclasses.Classes.Monoid.Monoid (Matrix a)
matrixConcatHorizontal =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend appendHorizontal) matrixEmpty


{-| Applicative pure for Matrix
-}
pure : a -> Matrix a
pure a =
    Matrix [ RowVector <| Vector.Vector <| [ a ] ]


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


{-| Print a matrix to a string
-}
print : Matrix Float -> String
print (Matrix listOfRowVectors) =
    let
        values =
            List.foldl (\(RowVector row) acc -> "RowVector " ++ Vector.print row ++ " ]" ++ acc) "" listOfRowVectors
    in
    "Matrix [ " ++ values ++ " ]"


{-| Try to read a string into a Matrix
-}
read : String -> Result (List Parser.DeadEnd) (Matrix Float)
read matrixString =
    Parser.run parseMatrix matrixString


listOfRowVectorParser : Parser.Parser (List (RowVector Float))
listOfRowVectorParser =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = parseRowVector
        , trailing = Parser.Forbidden
        }


parseMatrix : Parser.Parser (Matrix Float)
parseMatrix =
    Parser.succeed Matrix
        |. Parser.keyword "Matrix"
        |. Parser.spaces
        |= listOfRowVectorParser


parseRowVector : Parser.Parser (RowVector Float)
parseRowVector =
    Parser.succeed RowVector
        |. Parser.keyword "RowVector"
        |. Parser.spaces
        |= Vector.parseVector


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
            let
                numberOfRows =
                    mDimension squareMatrix

                indices =
                    List.Extra.initialize numberOfRows (\index -> ( index, index ))

                diagonalMaybeEntries =
                    List.foldl (\( indexOne, indexTwo ) acc -> getAt ( indexOne, indexTwo ) squareMatrix :: acc) [] indices
            in
            Maybe.Extra.combine diagonalMaybeEntries
                |> Maybe.map (\li -> List.foldl (\elem acc -> vectorSpace.abelianGroup.field.multiply elem acc) vectorSpace.abelianGroup.field.one li)
                |> Result.fromMaybe "Index out of range"
        )
        upperTriangularForm


{-| Try to calculate the inverse of a real numbered matrix
-}
invert : Vector.VectorSpace a -> Matrix a -> Result String (Matrix a)
invert vectorSpace matrix =
    case isInvertable vectorSpace matrix of
        Ok invertableMatrix ->
            let
                sizeOfMatrix =
                    mDimension invertableMatrix

                augmentedMatrix =
                    appendHorizontal invertableMatrix (identityMatrix vectorSpace.abelianGroup.field sizeOfMatrix)

                reducedRowEchelonForm =
                    gaussJordan vectorSpace augmentedMatrix

                inverse =
                    subMatrix 0 (mDimension reducedRowEchelonForm) sizeOfMatrix (nDimension reducedRowEchelonForm) reducedRowEchelonForm
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


{-| Determine whether a matirx is unitary
-}
isUnitary : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Bool
isUnitary matrix =
    case invert Vector.complexVectorSpace matrix of
        Ok inverse ->
            equal ComplexNumbers.equal inverse (adjoint matrix)

        Err _ ->
            False


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


isSquareMatrix : Matrix a -> Bool
isSquareMatrix matrix =
    if mDimension matrix == nDimension matrix then
        True

    else
        False


realMatrixAbelianGroup : AbelianGroup Float
realMatrixAbelianGroup =
    { field = Field.realField
    , addMatrcs = addMatrices Field.realField
    , subtractMatrcs = subtractMatrices Field.realField
    }
