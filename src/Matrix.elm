module Matrix exposing
    ( RowVector(..)
    , ColumnVector(..)
    , Matrix(..)
    , Solution(..)
    , Consistancy(..)
    , VectorDimension(..)
    , identity
    , zeros
    , scalarMultiplication
    , transpose
    , conjugate
    , adjoint
    , subMatrix
    , nullSpace
    , leftNullSpace
    , getDiagonal
    , getDiagonalProduct
    , rank
    , createMatrixFromColumnVectors
    , add
    , subtract
    , multiplyMatrixVector
    , multiply
    , tensorProduct
    , commuter
    , isOnto
    , isOneToOne
    , areBasis
    , areLinearlyIndependent
    , doesSetSpanSpace
    , mDimension
    , nDimension
    , areRowEquivalent
    , empty
    , concatHorizontal
    , concatVertical
    , realMatrixAdditionCommutativeSemigroup, complexMatrixAdditionCommutativeSemigroup
    , realMatrixAdditionCommutativeMonoid, complexMatrixAdditionCommutativeMonoid
    , realMatrixAlgebra
    , complexMatrixAdditionAbelianGroup
    , map
    , pure
    , andMap
    , andThen
    , map2
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
    , MatrixSpace, all, appendHorizontal, realMatrixAdditionGroup, realMatrixAdditionSemigroup, realMatrixSpace
    )

{-| A module for Matrix


# Types

@docs RowVector
@docs ColumnVector
@docs Matrix
@docs Solution
@docs Consistancy
@docs VectorDimension
@docs SquareMatrix
@docs InvertableMatrix
@docs HermitianMatrix
@docs UnitaryMatrix
@docs DoublyStochasticMatrix


# Values

@docs identity
@docs zeros
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
@docs normReal
@docs normComplex
@docs leftNullSpace
@docs getDiagonal
@docs getDiagonalProduct
@docs rank
@docs createMatrixFromColumnVectors


# Binary Operations

@docs add
@docs subtract
@docs multiplyMatrixVector
@docs multiply
@docs dotProduct
@docs tensorProduct
@docs commuter


# Matrix Predicates and Properties

@docs isSquareMatrix
@docs isSymmetric
@docs isHermitian
@docs isOnto
@docs isOneToOne
@docs isInvertable
@docs isUnitary
@docs areBasis
@docs areLinearlyIndependent
@docs doesSetSpanSpace
@docs mDimension
@docs nDimension
@docs isRightStochastic
@docs isLeftStochastic
@docs isDoublyStochastic
@docs areRowEquivalent


# Semigroup, Monoid, Group, Ring, Field, VectorSpace, InnerProductSpace

@docs empty
@docs concatHorizontal
@docs concatVertical
@docs realMatrixAdditionCommutativeSemigroup, complexMatrixAdditionCommutativeSemigroup
@docs realMatrixAdditionCommutativeMonoid, complexMatrixAdditionCommutativeMonoid
@docs realMatrixAlgebra
@docs realMatrixInnerProductSpace
@docs complexMatrixAdditionAbelianGroup


# Functor, Applicative, Monad, Foldable

@docs map
@docs pure
@docs andMap
@docs andThen
@docs map2
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

import AbelianGroup
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers
import Field
import Float.Extra
import Group
import Internal.Matrix
import List.Extra
import Maybe.Extra
import Monoid
import Parser exposing ((|.), (|=))
import Semigroup
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


{-| Type to represent a Vector Space
-}
type alias MatrixSpace a =
    { abelianGroup : AbelianGroup.AbelianGroup (Matrix a)
    , matrixScalarMultiplication : a -> Matrix a -> Matrix a
    }


{-| Type to represent a Matrix Algebra
-}
type alias MatrixAlgebra a =
    { matrixSpace : MatrixSpace a
    , multiply :
        Vector.InnerProductSpace a
        -> Matrix a
        -> Matrix a
        -> Result String (Matrix a)
    }


{-| Semigroup instance for Matrix under the addition operation with real values.
-}
realMatrixAdditionSemigroup : Semigroup.Semigroup (Matrix Float)
realMatrixAdditionSemigroup =
    add Field.float


{-| Semigroup instance for Matrix under the addition operation with complex values.
-}
complexMatrixAdditionSemigroup : Semigroup.Semigroup (Matrix (ComplexNumbers.ComplexNumber Float))
complexMatrixAdditionSemigroup =
    add ComplexNumbers.complexField


{-| Commutative Semigroup instance for Matrix under the addition operation with real values.
-}
realMatrixAdditionCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Matrix Float)
realMatrixAdditionCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realMatrixAdditionSemigroup


{-| Commutative Semigroup instance for Matrix under the addition operation with complex values.
-}
complexMatrixAdditionCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Matrix (ComplexNumbers.ComplexNumber Float))
complexMatrixAdditionCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexMatrixAdditionSemigroup


{-| Monoid instance for Matrix under the addition operation with real values.
-}
realMatrixAdditionMonoid : Monoid.Monoid (Matrix Float)
realMatrixAdditionMonoid =
    Monoid.semigroupAndIdentity realMatrixAdditionSemigroup empty


{-| Monoid instance for Matrix under the addition operation with complex values.
-}
complexMatrixAdditionMonoid : Monoid.Monoid (Matrix (ComplexNumbers.ComplexNumber Float))
complexMatrixAdditionMonoid =
    Monoid.semigroupAndIdentity complexMatrixAdditionSemigroup empty


{-| Commutative Monoid instance for Matrix under the addition operation with real values.
-}
realMatrixAdditionCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Matrix Float)
realMatrixAdditionCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMatrixAdditionMonoid


{-| Commutative Monoid instance for Matrix under the addition operation with complex values.
-}
complexMatrixAdditionCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Matrix (ComplexNumbers.ComplexNumber Float))
complexMatrixAdditionCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMatrixAdditionMonoid


{-| Group instance for Matrix under the addition operation with real values.
-}
realMatrixAdditionGroup : Group.Group (Matrix Float)
realMatrixAdditionGroup =
    { monoid = realMatrixAdditionMonoid
    , inverse = map Group.numberSum.inverse
    }


{-| Group instance for Matrix under the addition operation with complex values.
-}
complexMatrixAdditionGroup : Group.Group (Matrix (ComplexNumbers.ComplexNumber Float))
complexMatrixAdditionGroup =
    { monoid = complexMatrixAdditionMonoid
    , inverse = map ComplexNumbers.complexSumGroup.inverse
    }


{-| Abelian Group instance for Matrix under the addition operation with real values.
-}
realMatrixAdditionAbelianGroup : AbelianGroup.AbelianGroup (Matrix Float)
realMatrixAdditionAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMatrixAdditionMonoid
        , inverse = realMatrixAdditionGroup.inverse
        }


{-| Abelian Group instance for Matrix under the addition operation with complex values.
-}
complexMatrixAdditionAbelianGroup : AbelianGroup.AbelianGroup (Matrix (ComplexNumbers.ComplexNumber Float))
complexMatrixAdditionAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = complexMatrixAdditionMonoid
        , inverse = complexMatrixAdditionGroup.inverse
        }


{-| Create a Matrix from a list of Column Vectors
-}
createMatrixFromColumnVectors : List (ColumnVector a) -> Matrix a
createMatrixFromColumnVectors columnVectors =
    List.map (\(ColumnVector vector) -> RowVector vector) columnVectors
        |> Matrix
        |> transpose


{-| Create Identity Matrix with n dimension
-}
identity : Field.Field a -> Int -> Matrix a
identity (Field.Field field) dimension =
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
zeros : Field.Field a -> Int -> Int -> Matrix a
zeros (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) m n =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    List.Extra.initialize m
        (\_ ->
            Vector.zeros group.monoid n
                |> RowVector
        )
        |> Matrix


{-| Scalar multiplication over a Matrix
-}
scalarMultiplication : Field.Field a -> a -> Matrix a -> Matrix a
scalarMultiplication (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) scalar =
    map (commutativeDivisionRing.multiplication.monoid.semigroup scalar)


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
    Matrix (ComplexNumbers.ComplexNumber number)
    -> Matrix (ComplexNumbers.ComplexNumber number)
conjugate =
    map ComplexNumbers.conjugate


{-| Perform the adjoint operation on a Complex Numbered Matrix
-}
adjoint :
    Matrix (ComplexNumbers.ComplexNumber number)
    -> Matrix (ComplexNumbers.ComplexNumber number)
adjoint matrix =
    matrix
        |> map ComplexNumbers.conjugate
        |> transpose


{-| Calculate the rank of a Matrix
-}
rank : Vector.InnerProductSpace a -> Matrix a -> Int
rank innerProductSpace matrix =
    let
        (Matrix listOfRowVectorsREF) =
            gaussianReduce innerProductSpace.vectorSpace matrix
    in
    listOfRowVectorsREF
        |> List.Extra.count
            (\(RowVector vector) ->
                innerProductSpace.length vector /= 0
            )


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
nullSpace : Vector.InnerProductSpace a -> Matrix a -> Consistancy a
nullSpace innerProductSpace matrix =
    let
        numberOfRows =
            mDimension matrix

        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            innerProductSpace.vectorSpace.field

        (AbelianGroup.AbelianGroup additionGroup) =
            commutativeDivisionRing.addition

        b =
            List.repeat numberOfRows additionGroup.monoid.identity
                |> Vector.Vector
                |> ColumnVector
    in
    solve innerProductSpace matrix b


{-| Calculate the left nullspace of a Matrix
-}
leftNullSpace : Vector.InnerProductSpace a -> Matrix a -> Consistancy a
leftNullSpace innerProductSpace =
    transpose >> nullSpace innerProductSpace


{-| Get the diagonal of a Matrix
-}
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


{-| Get the Product of the diagonal of a Matrix
-}
getDiagonalProduct : Field.Field a -> Matrix a -> Maybe a
getDiagonalProduct (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) matrix =
    getDiagonal matrix
        |> Maybe.map
            (List.foldl
                (\elem acc ->
                    commutativeDivisionRing.multiplication.monoid.semigroup
                        elem
                        acc
                )
                commutativeDivisionRing.multiplication.monoid.identity
            )


{-| Add two Matrices together
-}
add : Field.Field a -> Matrix a -> Matrix a -> Matrix a
add (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    let
        (AbelianGroup.AbelianGroup groupAddition) =
            commutativeDivisionRing.addition
    in
    map2 groupAddition.monoid.semigroup


{-| Subtract two Matrices
-}
subtract : Field.Field a -> Matrix a -> Matrix a -> Matrix a
subtract (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    let
        (AbelianGroup.AbelianGroup groupAddition) =
            commutativeDivisionRing.addition
    in
    map2 (\x y -> groupAddition.monoid.semigroup x (groupAddition.inverse y))


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
multiply :
    Vector.InnerProductSpace a
    -> Matrix a
    -> Matrix a
    -> Result String (Matrix a)
multiply innerProductSpace (Matrix matrixOne) matrixTwo =
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


{-| Calculate the tensor product of two Matricies
-}
tensorProduct : Field.Field a -> Matrix a -> Matrix a -> Matrix a
tensorProduct field matrixOne matrixTwo =
    andThen
        (\matrixOneElement ->
            scalarMultiplication field matrixOneElement matrixTwo
        )
        matrixOne


{-| Map over a Matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix listOfRowVectors) =
    Matrix <| List.map (rowVectorMap f) listOfRowVectors


{-| Lift a binary function to work on Matrix
-}
map2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
map2 f (Matrix listOfRowVectorsOne) (Matrix listOfRowVectorsTwo) =
    List.map2 (rowVectorMap2 f)
        listOfRowVectorsOne
        listOfRowVectorsTwo
        |> Matrix


{-| Applicative pure for Matrix
-}
pure : a -> Matrix a
pure a =
    Matrix [ RowVector <| Vector.Vector <| [ a ] ]


{-| Apply for Matrix
-}
andMap : Matrix a -> Matrix (a -> b) -> Matrix b
andMap fMatrix matrix =
    map2 Basics.identity matrix fMatrix


{-| Monad bind for Matrix
-}
andThen : (a -> Matrix b) -> Matrix a -> Matrix b
andThen fMatrix (Matrix listOfRowVectors) =
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


{-| Determine whether a matirx is onto
-}
isOnto : Vector.InnerProductSpace a -> Matrix a -> Result String (Matrix a)
isOnto innerProductSpace matrix =
    if rank innerProductSpace matrix == mDimension matrix then
        Ok matrix

    else
        Err "Matrix not onto"


{-| Determine whether a matirx is one to one
-}
isOneToOne : Vector.InnerProductSpace a -> Matrix a -> Result String (Matrix a)
isOneToOne innerProductSpace matrix =
    if rank innerProductSpace matrix == nDimension matrix then
        Ok matrix

    else
        Err "Matrix not one to one"


{-| Predicate to determine if two matricies are row equivalent
-}
areRowEquivalent : Vector.VectorSpace a -> Matrix a -> Matrix a -> Bool
areRowEquivalent vectorSpace matrixOne matrixTwo =
    gaussJordan vectorSpace matrixOne == gaussJordan vectorSpace matrixTwo


all : (a -> Bool) -> Matrix a -> Bool
all predicate (Matrix listOfRowVectors) =
    List.map (rowVectorAll predicate) listOfRowVectors
        |> List.all ((==) True)


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : Vector.VectorSpace a -> Matrix a -> Matrix a
upperTriangle vectorSpace (Matrix matrix) =
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


{-| Gaussian Elimination
-}
gaussianReduce : Vector.VectorSpace a -> Matrix a -> Matrix a
gaussianReduce vectorSpace (Matrix matrix) =
    let
        (Matrix upperTriangularFormRectangle) =
            upperTriangle vectorSpace (Matrix matrix)

        listOfVectorsUpperTriangularFormRectangle =
            List.map
                (\(RowVector vector) -> vector)
                upperTriangularFormRectangle

        rowEchelonForm =
            List.indexedMap
                (Internal.Matrix.scale vectorSpace)
                listOfVectorsUpperTriangularFormRectangle
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
solveMatrix : Vector.InnerProductSpace a -> Matrix a -> Consistancy a
solveMatrix innerProductSpace (Matrix listOfRowVectors) =
    let
        (Matrix listOfRowVectorsRREF) =
            gaussJordan innerProductSpace.vectorSpace (Matrix listOfRowVectors)

        (Matrix variableSide) =
            coefficientMatrix (Matrix listOfRowVectorsRREF)

        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            innerProductSpace.vectorSpace.field

        (AbelianGroup.AbelianGroup additionGroup) =
            commutativeDivisionRing.addition

        notConstrainedEnough =
            variableSide
                |> List.any
                    (\(RowVector (Vector.Vector row)) ->
                        let
                            countOfOnes =
                                List.Extra.count ((/=) additionGroup.monoid.identity) row
                        in
                        countOfOnes > 1
                    )

        anyAllZeroExceptAugmentedSide =
            listOfRowVectorsRREF
                |> List.any
                    (\(RowVector (Vector.Vector row)) ->
                        List.all
                            ((==) additionGroup.monoid.identity)
                            (List.take (List.length row - 1) row)
                            && innerProductSpace.length (Vector.Vector row)
                            /= 0
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
            rnk =
                rank innerProductSpace (Matrix listOfRowVectorsRREF)

            nullity =
                nDimension (Matrix listOfRowVectorsRREF) - rnk
        in
        InfiniteSolutions { nullity = nullity, rank = rnk }
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
solve : Vector.InnerProductSpace a -> Matrix a -> ColumnVector a -> Consistancy a
solve innerProductSpace matrix (ColumnVector (Vector.Vector constants)) =
    let
        matrixB =
            constants
                |> List.map (List.singleton >> Vector.Vector >> RowVector)
                |> Matrix

        augmentedMatrix =
            concatHorizontal.semigroup.prepend matrix matrixB
    in
    solveMatrix innerProductSpace augmentedMatrix


{-| Predicate to determine if a list of Vectors are linearly independent
-}
areLinearlyIndependent : Vector.InnerProductSpace a -> List (Vector.Vector a) -> Bool
areLinearlyIndependent innerProductSpace listOfVectors =
    let
        matrix =
            List.map RowVector listOfVectors
                |> Matrix
    in
    rank innerProductSpace matrix == nDimension matrix


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
                identity vSpace.field vectorDimension

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
areBasis : Vector.InnerProductSpace a -> VectorDimension -> List (Vector.Vector a) -> Bool
areBasis innerProductSpace vectorDimension vectors =
    doesSetSpanSpace innerProductSpace.vectorSpace vectorDimension vectors
        == Ok True
        && areLinearlyIndependent innerProductSpace vectors


rowVectorMap : (a -> b) -> RowVector a -> RowVector b
rowVectorMap f (RowVector vector) =
    Vector.map f vector
        |> RowVector


rowVectorMap2 : (a -> b -> c) -> RowVector a -> RowVector b -> RowVector c
rowVectorMap2 f (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.map2 f vectorOne vectorTwo
        |> RowVector


{-| Left fold over a RowVector
-}
rowVectorFoldl : (a -> b -> b) -> b -> RowVector a -> b
rowVectorFoldl foldFunction acc (RowVector vector) =
    Vector.foldl foldFunction acc vector


rowVectorAll : (a -> Bool) -> RowVector a -> Bool
rowVectorAll predicate (RowVector vector) =
    Vector.all predicate vector


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
empty : Matrix a
empty =
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
concatVertical : Typeclasses.Classes.Monoid.Monoid (Matrix a)
concatVertical =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend appendVertical)
        empty


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
concatHorizontal : Typeclasses.Classes.Monoid.Monoid (Matrix a)
concatHorizontal =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend appendHorizontal)
        empty


{-| Compare two Matrices for equality
-}
equalImplementation : (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
equalImplementation comparator matrixOne matrixTwo =
    map2
        comparator
        matrixOne
        matrixTwo
        |> all
            ((==) True)


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
    "Matrix [ "
        ++ List.foldl
            (\(RowVector row) acc -> "RowVector " ++ Vector.printRealVector row ++ " ]" ++ acc)
            ""
            listOfRowVectors
        ++ " ]"


{-| Print a Complex matrix to a string
-}
printComplexMatrix : Matrix (ComplexNumbers.ComplexNumber Float) -> String
printComplexMatrix (Matrix listOfRowVectors) =
    "Matrix [ "
        ++ List.foldl
            (\(RowVector row) acc -> "RowVector " ++ Vector.printComplexVector row ++ " ]" ++ acc)
            ""
            listOfRowVectors
        ++ " ]"


{-| Try to read a string into a Matrix
-}
readRealMatrix : String -> Result (List Parser.DeadEnd) (Matrix Float)
readRealMatrix matrixString =
    Parser.run (parseMatrix Vector.negativeOrPositiveFloat) matrixString


{-| Try to read a string into a Matrix
-}
readComplexMatrix : String -> Result (List Parser.DeadEnd) (Matrix (ComplexNumbers.ComplexNumber Float))
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


{-| Real Numbered Vector Space for Matrix
-}
realMatrixSpace : MatrixSpace Float
realMatrixSpace =
    { abelianGroup = realMatrixAdditionAbelianGroup
    , matrixScalarMultiplication = scalarMultiplication Field.float
    }


{-| Complex Numbered Vector Space for Matrix
-}
complexMatrixSpace : MatrixSpace (ComplexNumbers.ComplexNumber Float)
complexMatrixSpace =
    { abelianGroup = complexMatrixAdditionAbelianGroup
    , matrixScalarMultiplication = scalarMultiplication ComplexNumbers.complexField
    }


{-| Real Numbered Matrix Algebra
-}
realMatrixAlgebra : MatrixAlgebra Float
realMatrixAlgebra =
    { matrixSpace = realMatrixSpace
    , multiply = multiply
    }


{-| Complex Numbered Matrix Algebra
-}
complexMatrixAlgebra : MatrixAlgebra (ComplexNumbers.ComplexNumber Float)
complexMatrixAlgebra =
    { matrixSpace = complexMatrixSpace
    , multiply = multiply
    }


{-| Calculate the commuter of two Hermitian Matricies
-}
commuter : Vector.InnerProductSpace a -> Matrix a -> Matrix a -> Result String (Matrix a)
commuter innerProductSpace matrixOne matrixTwo =
    Result.map2 (subtract innerProductSpace.vectorSpace.field)
        (multiply innerProductSpace matrixOne matrixTwo)
        (multiply innerProductSpace matrixTwo matrixOne)
