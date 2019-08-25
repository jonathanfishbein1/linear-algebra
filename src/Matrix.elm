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
    , gaussianReduceComplex, invert, jordanReduceComplex, upperTriangleComplex
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

-}

import ComplexNumbers
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
sumRealMatrices : Matrix number -> Typeclasses.Classes.Monoid.Monoid (Matrix number)
sumRealMatrices sumEmptyMatrix =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend addRealMatrices) sumEmptyMatrix


{-| Monoidally add two Complex numbered Matrices together
-}
sumComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian number) -> Typeclasses.Classes.Monoid.Monoid (Matrix (ComplexNumbers.ComplexNumberCartesian number))
sumComplexMatrices sumEmptyMatrix =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend addComplexMatrices) sumEmptyMatrix


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


{-| Matrix Matrix multiplication for a Complex Numbered Matrix
-}
multiplyComplexMatrices : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Result String (Matrix (ComplexNumbers.ComplexNumberCartesian Float))
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
        Internal.Matrix.map2VectorCartesianComplex listOfVectorsOne listOfVectors
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
        Internal.Matrix.map2VectorCartesian listOfVectorsOne listOfVectors
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
multiplyRealVectorRealMatrix : Matrix number -> Vector.Vector number -> Vector.Vector number
multiplyRealVectorRealMatrix (Matrix matrix) vector =
    let
        listOfVectors =
            matrix
                |> List.map (\(RowVector vec) -> vec)
    in
    Internal.Matrix.map2VectorCartesian listOfVectors [ vector ]
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
gaussianReduce : Matrix Float -> Matrix Float
gaussianReduce (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix

        upperTriangularFormRectangle =
            List.foldl Internal.Matrix.calculateUpperTriangularFormRectangle listOfVectors (List.range 0 (List.length matrix - 1))

        rowEchelonForm =
            List.indexedMap
                Internal.Matrix.scale
                upperTriangularFormRectangle
    in
    rowEchelonForm
        |> List.map RowVector
        |> Matrix


{-| Gaussian Elimination
-}
gaussianReduceComplex : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Matrix (ComplexNumbers.ComplexNumberCartesian Float)
gaussianReduceComplex (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix

        upperTriangularFormRectangleComplex =
            List.foldl Internal.Matrix.calculateUpperTriangularFormRectangleComplex listOfVectors (List.range 0 (List.length matrix - 1))

        rowEchelonForm =
            List.indexedMap
                (\index row -> Debug.log "scaledRow " <| Internal.Matrix.scaleComplex index row)
                upperTriangularFormRectangleComplex
    in
    rowEchelonForm
        |> List.map RowVector
        |> Matrix


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : Matrix Float -> Result String (Matrix Float)
upperTriangle (Matrix matrix) =
    if mDimension (Matrix matrix) == nDimension (Matrix matrix) then
        let
            listOfVectors =
                List.map (\(RowVector vector) -> vector) matrix
        in
        List.foldl Internal.Matrix.calculateUpperTriangularFormRectangle listOfVectors (List.range 0 (List.length matrix - 1))
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "Must be Square Matrix"


{-| Put a matrix into Upper Triangular Form
-}
upperTriangleComplex : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Result String (Matrix (ComplexNumbers.ComplexNumberCartesian Float))
upperTriangleComplex (Matrix matrix) =
    if mDimension (Matrix matrix) == nDimension (Matrix matrix) then
        let
            listOfVectors =
                List.map (\(RowVector vector) -> vector) matrix
        in
        List.foldl Internal.Matrix.calculateUpperTriangularFormRectangleComplex listOfVectors (List.range 0 (List.length matrix - 1))
            |> List.map RowVector
            |> Matrix
            |> Ok

    else
        Err "Must be Square Matrix"


{-| Internal function for Jordan Elimination
-}
jordanReduce : Matrix Float -> Matrix Float
jordanReduce (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix
    in
    List.foldl Internal.Matrix.reduceRowBackwards listOfVectors (List.reverse (List.range 0 (List.length matrix - 1)))
        |> List.map RowVector
        |> Matrix


{-| Internal function for Jordan Elimination
-}
jordanReduceComplex : Matrix (ComplexNumbers.ComplexNumberCartesian Float) -> Matrix (ComplexNumbers.ComplexNumberCartesian Float)
jordanReduceComplex (Matrix matrix) =
    let
        listOfVectors =
            List.map (\(RowVector vector) -> vector) matrix
    in
    List.foldl Internal.Matrix.reduceRowBackwardsComplex listOfVectors (List.reverse (List.range 0 (List.length matrix - 1)))
        |> List.map RowVector
        |> Matrix


{-| Function composition of Gaussian Elimination and Jordan Elimination
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
                |> List.map (List.singleton >> Vector.Vector >> RowVector)
                |> Matrix

        augmentedMatrix =
            matrixConcatHorizontal.semigroup.prepend matrix matrixB
    in
    solveMatrix augmentedMatrix


variablePortion : Matrix Float -> Matrix Float
variablePortion matrix =
    subMatrix 0 (mDimension matrix) 0 (nDimension matrix - 1) matrix


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
            List.repeat numberOfRows 0
                |> Vector.Vector
                |> ColumnVector
    in
    solve matrix b


{-| Predicate to determine if a list of Vectors are linearly independent
-}
areLinearlyIndependent : List (Vector.Vector Float) -> Bool
areLinearlyIndependent listOfVectors =
    let
        listOfRowVectors =
            List.map RowVector listOfVectors

        matrix =
            Matrix listOfRowVectors

        matrixNullSpace =
            nullSpace matrix

        numberOfRows =
            List.length listOfRowVectors

        zeroVector =
            List.repeat numberOfRows 0
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
doesSetSpanSpace : VectorSpace -> List (Vector.Vector Float) -> Result String Bool
doesSetSpanSpace (VectorSpace vectorSpace) vectors =
    if List.length vectors /= vectorSpace then
        Err "Please input same number of vectors as vector space"

    else if not <| List.all (\vector -> Vector.dimension vector == vectorSpace) vectors then
        Err "Please input vectors of equal length as vector space"

    else
        let
            identityRowVectors =
                identityMatrix vectorSpace

            floatMatrix =
                identityRowVectors
                    |> map toFloat

            listOfRowVectorsRREF =
                gaussJordan (Matrix (List.map RowVector vectors))
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
areBasis : VectorSpace -> List (Vector.Vector Float) -> Bool
areBasis vectorSpace vectors =
    if doesSetSpanSpace vectorSpace vectors == Ok True && areLinearlyIndependent vectors then
        True

    else
        False


{-| Determine the basis vectors of a vector space
-}
basisOfVectorSpace : VectorSpace -> List (Vector.Vector Float) -> List (Vector.Vector Float)
basisOfVectorSpace vectorSpace vectors =
    if areBasis vectorSpace vectors then
        vectors

    else
        let
            (Matrix reducedRowEchelonFormListOfRowVectors) =
                jordanReduce (Matrix (List.map RowVector vectors))
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
determinant : Matrix Float -> Result String Float
determinant matrix =
    let
        upperTriangularForm =
            upperTriangle matrix
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
                |> Maybe.map List.product
                |> Result.fromMaybe "Index out of range"
        )
        upperTriangularForm


invert : Matrix Float -> Result String (Matrix Float)
invert matrix =
    let
        theDeterminant =
            determinant matrix
    in
    case theDeterminant of
        Ok value ->
            if Float.Extra.equalWithin 0.000000001 value 0.0 then
                Err "Determinant is zero matrix is not invertable"

            else
                let
                    sizeOfMatrix =
                        mDimension matrix

                    augmentedMatrix =
                        appendHorizontal matrix (identityMatrix sizeOfMatrix |> map toFloat)

                    reducedRowEchelonForm =
                        gaussJordan augmentedMatrix

                    inverse =
                        subMatrix 0 (mDimension reducedRowEchelonForm) sizeOfMatrix (nDimension reducedRowEchelonForm) reducedRowEchelonForm
                in
                Ok inverse

        Err err ->
            Err err


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
