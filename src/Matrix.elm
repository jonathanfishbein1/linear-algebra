module Matrix exposing
    ( Matrix(..)
    , Solution(..)
    , Consistancy(..)
    , VectorDimension(..)
    , MatrixSpace
    , identity
    , zeros
    , createMatrixFromColumnVectors
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
    , add
    , subtract
    , multiplyMatrixVector
    , multiply
    , tensorProduct
    , commuter
    , areBasis
    , areLinearlyIndependent
    , doesSetSpanSpace
    , mDimension
    , nDimension
    , areRowEquivalent
    , all
    , isOneToOne
    , isOnto
    , realAdditionSemigroup
    , complexAdditionSemigroup
    , realAdditionCommutativeSemigroup
    , complexAdditionCommutativeSemigroup
    , realAdditionCommutativeMonoid
    , complexAdditionCommutativeMonoid
    , realMatrixAlgebra
    , complexMatrixAlgebra
    , realAdditionGroup
    , complexAdditionGroup
    , realAdditionAbelianGroup
    , complexAdditionAbelianGroup
    , realMatrixSpace
    , complexMatrixSpace
    , empty
    , concatHorizontal
    , concatVertical
    , appendHorizontal
    , map
    , pure
    , andMap
    , andThen
    , map2
    , foldl
    , equal
    , equalImplementation
    , upperTriangle
    , gaussianReduce
    , jordanReduce
    , gaussJordan
    , solve
    , getAt
    , setAt
    , printRealMatrix
    , printComplexMatrix
    , readRealMatrix
    , readComplexMatrix
    , multiplyIfCan
    )

{-| A module for Matrix


# Types

@docs Matrix
@docs Solution
@docs Consistancy
@docs VectorDimension
@docs MatrixSpace


# Values

@docs identity
@docs zeros


# Constructors

@docs createMatrixFromColumnVectors


# Unitary Operations

@docs scalarMultiplication
@docs transpose
@docs conjugate
@docs adjoint
@docs subMatrix
@docs nullSpace
@docs leftNullSpace
@docs getDiagonal
@docs getDiagonalProduct
@docs rank


# Binary Operations

@docs add
@docs subtract
@docs multiplyMatrixVector
@docs multiply
@docs tensorProduct
@docs commuter


# Matrix Predicates and Properties

@docs areBasis
@docs areLinearlyIndependent
@docs doesSetSpanSpace
@docs mDimension
@docs nDimension
@docs areRowEquivalent
@docs all
@docs isOneToOne
@docs isOnto


# Semigroup, Monoid, Group, Ring, Field, VectorSpace instances

@docs realAdditionSemigroup
@docs complexAdditionSemigroup
@docs realAdditionCommutativeSemigroup
@docs complexAdditionCommutativeSemigroup
@docs realAdditionCommutativeMonoid
@docs complexAdditionCommutativeMonoid
@docs realMatrixAlgebra
@docs complexMatrixAlgebra
@docs realAdditionGroup
@docs complexAdditionGroup
@docs realAdditionAbelianGroup
@docs complexAdditionAbelianGroup
@docs realMatrixSpace
@docs complexMatrixSpace


# Monoid, Functor, Applicative, Monad, Foldable functions

@docs empty
@docs concatHorizontal
@docs concatVertical
@docs appendHorizontal
@docs map
@docs pure
@docs andMap
@docs andThen
@docs map2
@docs foldl


# Equality

@docs equal
@docs equalImplementation


# Matrix Forms

@docs upperTriangle
@docs gaussianReduce
@docs jordanReduce
@docs gaussJordan


# Solving

@docs solve


# Manipulation

@docs getAt
@docs setAt
@docs printRealMatrix
@docs printComplexMatrix
@docs readRealMatrix
@docs readComplexMatrix

-}

import AbelianGroup
import ColumnVector
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers
import Field
import Group
import Internal.Matrix
import Internal.Vector
import List.Extra
import Maybe.Extra
import Monoid
import Parser exposing ((|.), (|=))
import Real
import RowVector
import Semigroup
import Typeclasses.Classes.Equality
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup
import Vector


{-| Matrix type
-}
type Matrix a
    = Matrix (List (RowVector.RowVector a))


{-| Type to represent result of Gauss-Jordan reduction
-}
type Consistancy a
    = Consistant (Solution a)
    | Inconsistant String (Solution a)


{-| Type to represent result of Gauss-Jordan reduction if system is consistant
-}
type Solution a
    = UniqueSolution (ColumnVector.ColumnVector a)
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
        RowVector.InnerProductSpace a
        -> Matrix a
        -> Matrix a
        -> Result String (Matrix a)
    }


{-| Semigroup instance for Matrix under the addition operation with real values.
-}
realAdditionSemigroup : Semigroup.Semigroup (Matrix (Real.Real Float))
realAdditionSemigroup =
    add Real.field


{-| Semigroup instance for Matrix under the addition operation with complex values.
-}
complexAdditionSemigroup : Semigroup.Semigroup (Matrix (ComplexNumbers.ComplexNumber Float))
complexAdditionSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for Matrix under the addition operation with real values.
-}
realAdditionCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Matrix (Real.Real Float))
realAdditionCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realAdditionSemigroup


{-| Commutative Semigroup instance for Matrix under the addition operation with complex values.
-}
complexAdditionCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Matrix (ComplexNumbers.ComplexNumber Float))
complexAdditionCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexAdditionSemigroup


{-| Monoid instance for Matrix under the addition operation with real values.
-}
realAdditionMonoid : Monoid.Monoid (Matrix (Real.Real Float))
realAdditionMonoid =
    Monoid.semigroupAndIdentity realAdditionSemigroup empty


{-| Monoid instance for Matrix under the addition operation with complex values.
-}
complexAdditionMonoid : Monoid.Monoid (Matrix (ComplexNumbers.ComplexNumber Float))
complexAdditionMonoid =
    Monoid.semigroupAndIdentity complexAdditionSemigroup empty


{-| Commutative Monoid instance for Matrix under the addition operation with real values.
-}
realAdditionCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Matrix (Real.Real Float))
realAdditionCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realAdditionMonoid


{-| Commutative Monoid instance for Matrix under the addition operation with complex values.
-}
complexAdditionCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Matrix (ComplexNumbers.ComplexNumber Float))
complexAdditionCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexAdditionMonoid


{-| Group instance for Matrix under the addition operation with real values.
-}
realAdditionGroup : Group.Group (Matrix (Real.Real Float))
realAdditionGroup =
    { monoid = realAdditionMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for Matrix under the addition operation with complex values.
-}
complexAdditionGroup : Group.Group (Matrix (ComplexNumbers.ComplexNumber Float))
complexAdditionGroup =
    { monoid = complexAdditionMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for Matrix under the addition operation with real values.
-}
realAdditionAbelianGroup : AbelianGroup.AbelianGroup (Matrix (Real.Real Float))
realAdditionAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realAdditionMonoid
        , inverse = realAdditionGroup.inverse
        }


{-| Abelian Group instance for Matrix under the addition operation with complex values.
-}
complexAdditionAbelianGroup : AbelianGroup.AbelianGroup (Matrix (ComplexNumbers.ComplexNumber Float))
complexAdditionAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = complexAdditionMonoid
        , inverse = complexAdditionGroup.inverse
        }


{-| Real Numbered Vector Space for Matrix
-}
realMatrixSpace : MatrixSpace (Real.Real Float)
realMatrixSpace =
    { abelianGroup = realAdditionAbelianGroup
    , matrixScalarMultiplication = scalarMultiplication Real.field
    }


{-| Complex Numbered Vector Space for Matrix
-}
complexMatrixSpace : MatrixSpace (ComplexNumbers.ComplexNumber Float)
complexMatrixSpace =
    { abelianGroup = complexAdditionAbelianGroup
    , matrixScalarMultiplication = scalarMultiplication ComplexNumbers.field
    }


{-| Real Numbered Matrix Algebra
-}
realMatrixAlgebra : MatrixAlgebra (Real.Real Float)
realMatrixAlgebra =
    { matrixSpace = realMatrixSpace
    , multiply = multiplyIfCan
    }


{-| Complex Numbered Matrix Algebra
-}
complexMatrixAlgebra : MatrixAlgebra (ComplexNumbers.ComplexNumber Float)
complexMatrixAlgebra =
    { matrixSpace = complexMatrixSpace
    , multiply = multiplyIfCan
    }


{-| Create a Matrix from a list of Column Vectors
-}
createMatrixFromColumnVectors : List (ColumnVector.ColumnVector a) -> Matrix a
createMatrixFromColumnVectors =
    List.map (\(ColumnVector.ColumnVector vector) -> RowVector.RowVector vector)
        >> Matrix
        >> transpose


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
                    |> RowVector.RowVector
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
            Internal.Vector.zeros group.monoid n
                |> RowVector.RowVector
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
        |> List.map (\(RowVector.RowVector (Vector.Vector x)) -> x)
        |> List.Extra.transpose
        |> List.map (Vector.Vector >> RowVector.RowVector)
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
adjoint =
    conjugate
        >> transpose


{-| Calculate the rank of a Matrix
-}
rank : RowVector.InnerProductSpace a -> Matrix a -> Int
rank innerProductSpace matrix =
    let
        (Matrix listOfRowVectorsREF) =
            gaussianReduce innerProductSpace.vectorSpace matrix
    in
    listOfRowVectorsREF
        |> List.Extra.count
            (\rowVector ->
                innerProductSpace.length rowVector /= Real.zero
            )


{-| Calculate the submatrix given a starting and ending row and column index
-}
subMatrix : Int -> Int -> Int -> Int -> Matrix a -> Matrix a
subMatrix startingRowIndex endingRowIndex startingColumnIndex endingColumnIndex (Matrix listOfRowVectors) =
    List.take endingRowIndex listOfRowVectors
        |> List.drop startingRowIndex
        |> List.map
            (\(RowVector.RowVector (Vector.Vector row)) ->
                List.take endingColumnIndex row
                    |> List.drop startingColumnIndex
                    |> Vector.Vector
                    |> RowVector.RowVector
            )
        |> Matrix


{-| Calculate the null space of a matrix
-}
nullSpace : Typeclasses.Classes.Equality.Equality a -> RowVector.InnerProductSpace a -> Matrix a -> Consistancy a
nullSpace eq innerProductSpace matrix =
    let
        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            innerProductSpace.vectorSpace.field

        (AbelianGroup.AbelianGroup additionGroup) =
            commutativeDivisionRing.addition

        b =
            List.repeat (mDimension matrix) additionGroup.monoid.identity
                |> Vector.Vector
                |> ColumnVector.ColumnVector
    in
    solve eq innerProductSpace matrix b


{-| Calculate the left nullspace of a Matrix
-}
leftNullSpace : Typeclasses.Classes.Equality.Equality a -> RowVector.InnerProductSpace a -> Matrix a -> Consistancy a
leftNullSpace eq innerProductSpace =
    transpose >> nullSpace eq innerProductSpace


{-| Get the diagonal of a Matrix
-}
getDiagonal : Matrix a -> Maybe (List a)
getDiagonal matrix =
    let
        indices =
            List.Extra.initialize
                (mDimension matrix)
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
getDiagonalProduct (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
    getDiagonal
        >> Maybe.map
            (List.foldl
                commutativeDivisionRing.multiplication.monoid.semigroup
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
    RowVector.InnerProductSpace a
    -> Matrix a
    -> ColumnVector.ColumnVector a
    -> Result String (ColumnVector.ColumnVector a)
multiplyMatrixVector innerProductSpace (Matrix listOfRowVector) (ColumnVector.ColumnVector columnVector) =
    if nDimension (Matrix listOfRowVector) == Internal.Vector.dimension columnVector then
        Internal.Matrix.map2VectorCartesian innerProductSpace listOfRowVector [ RowVector.RowVector columnVector ]
            |> List.foldl
                (\(RowVector.RowVector (Vector.Vector elem)) acc -> acc ++ elem)
                []
            |> Vector.Vector
            |> ColumnVector.ColumnVector
            |> Ok

    else
        Err "Matrix has to have the same number of columns as the vector has rows"


{-| Matrix Matrix multiplication
-}
multiply :
    RowVector.InnerProductSpace a
    -> Matrix a
    -> Matrix a
    -> Matrix a
multiply innerProductSpace (Matrix listOfVectorsOne) matrixTwo =
    let
        (Matrix listOfVectorsTranspose) =
            transpose matrixTwo
    in
    Internal.Matrix.map2VectorCartesian
        innerProductSpace
        listOfVectorsOne
        listOfVectorsTranspose
        |> Matrix


{-| Matrix Matrix multiplication
-}
multiplyIfCan :
    RowVector.InnerProductSpace a
    -> Matrix a
    -> Matrix a
    -> Result String (Matrix a)
multiplyIfCan innerProductSpace (Matrix matrixOne) matrixTwo =
    if nDimension (Matrix matrixOne) == mDimension matrixTwo then
        multiply innerProductSpace (Matrix matrixOne) matrixTwo
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
    Matrix <| List.map (RowVector.map f) listOfRowVectors


{-| Lift a binary function to work on Matrix
-}
map2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
map2 f (Matrix listOfRowVectorsOne) (Matrix listOfRowVectorsTwo) =
    List.map2 (RowVector.map2 f)
        listOfRowVectorsOne
        listOfRowVectorsTwo
        |> Matrix


{-| Applicative pure for Matrix
-}
pure : a -> Matrix a
pure a =
    Matrix [ RowVector.pure a ]


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
        (\(RowVector.RowVector (Vector.Vector listOfElements)) ->
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
isOnto : RowVector.InnerProductSpace a -> Matrix a -> Result String (Matrix a)
isOnto innerProductSpace matrix =
    if rank innerProductSpace matrix == mDimension matrix then
        Ok matrix

    else
        Err "Matrix not onto"


{-| Determine whether a matirx is one to one
-}
isOneToOne : RowVector.InnerProductSpace a -> Matrix a -> Result String (Matrix a)
isOneToOne innerProductSpace matrix =
    if rank innerProductSpace matrix == nDimension matrix then
        Ok matrix

    else
        Err "Matrix not one to one"


{-| Predicate to determine if two matricies are row equivalent
-}
areRowEquivalent : RowVector.VectorSpace a -> Matrix a -> Matrix a -> Bool
areRowEquivalent vectorSpace matrixOne matrixTwo =
    gaussJordan vectorSpace matrixOne == gaussJordan vectorSpace matrixTwo


{-| Predicate to determine if all values in the matric satisfy the given predicate
-}
all : (a -> Bool) -> Matrix a -> Bool
all predicate (Matrix listOfRowVectors) =
    List.map (RowVector.all predicate) listOfRowVectors
        |> List.all ((==) True)


{-| Put a matrix into Upper Triangular Form
-}
upperTriangle : RowVector.VectorSpace a -> Matrix a -> Matrix a
upperTriangle vectorSpace (Matrix listOfRowVectors) =
    List.foldl
        (Internal.Matrix.calculateUpperTriangularFormRectangle vectorSpace)
        listOfRowVectors
        (List.range 0 (List.length listOfRowVectors - 1))
        |> Matrix


{-| Gaussian Elimination
-}
gaussianReduce : RowVector.VectorSpace a -> Matrix a -> Matrix a
gaussianReduce vectorSpace matrix =
    let
        (Matrix upperTriangularFormRectangle) =
            upperTriangle vectorSpace matrix

        rowEchelonForm =
            List.indexedMap
                (Internal.Matrix.scale vectorSpace)
                upperTriangularFormRectangle
    in
    rowEchelonForm
        |> Matrix


{-| Internal function for Jordan Elimination
-}
jordanReduce : RowVector.VectorSpace a -> Matrix a -> Matrix a
jordanReduce vectorSpace (Matrix listOfRowVectors) =
    List.foldl
        (Internal.Matrix.reduceRowBackwards vectorSpace)
        listOfRowVectors
        (List.reverse (List.range 0 (List.length listOfRowVectors - 1)))
        |> Matrix


{-| Function composition of Gaussian Elimination and Jordan Elimination
-}
gaussJordan : RowVector.VectorSpace a -> Matrix a -> Matrix a
gaussJordan vectorSpace =
    gaussianReduce vectorSpace
        >> jordanReduce vectorSpace


coefficientMatrix : Matrix a -> Matrix a
coefficientMatrix matrix =
    subMatrix 0 (mDimension matrix) 0 (nDimension matrix - 1) matrix


{-| Solve a system of linear equations using Gauss-Jordan elimination with explict column vector of constants
-}
solve : Typeclasses.Classes.Equality.Equality a -> RowVector.InnerProductSpace a -> Matrix a -> ColumnVector.ColumnVector a -> Consistancy a
solve { eq } innerProductSpace matrix constants =
    let
        matrixB =
            createMatrixFromColumnVectors [ constants ]

        augmentedMatrix =
            concatHorizontal.semigroup.prepend matrix matrixB

        (Matrix listOfRowVectorsRREF) =
            gaussJordan innerProductSpace.vectorSpace augmentedMatrix

        (Matrix variableSide) =
            coefficientMatrix (Matrix listOfRowVectorsRREF)

        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            innerProductSpace.vectorSpace.field

        (AbelianGroup.AbelianGroup additionGroup) =
            commutativeDivisionRing.addition

        notConstrainedEnough =
            variableSide
                |> List.any
                    (\row ->
                        RowVector.count ((/=) additionGroup.monoid.identity) row > 1
                    )

        anyAllZeroExceptAugmentedSide =
            listOfRowVectorsRREF
                |> List.any
                    (\(RowVector.RowVector (Vector.Vector row)) ->
                        List.all
                            (eq additionGroup.monoid.identity)
                            (List.take (List.length row - 1) row)
                            && innerProductSpace.length (RowVector.RowVector (Vector.Vector row))
                            /= Real.zero
                    )

        solution =
            List.foldl
                (\(RowVector.RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row)
                []
                listOfRowVectorsRREF
    in
    if anyAllZeroExceptAugmentedSide then
        let
            matrixTransposeMatrix =
                multiplyIfCan innerProductSpace (transpose matrix) matrix

            matrixTransposeB =
                multiplyMatrixVector innerProductSpace (transpose matrix) constants
                    |> Result.map (List.singleton >> createMatrixFromColumnVectors)

            augmentedMatrixStar =
                Result.map2 concatHorizontal.semigroup.prepend matrixTransposeMatrix matrixTransposeB

            (Matrix listOfRowVectorsRREFStar) =
                Result.map (gaussJordan innerProductSpace.vectorSpace) augmentedMatrixStar
                    |> Result.withDefault empty

            solutionStar =
                List.foldl
                    (\(RowVector.RowVector (Vector.Vector row)) acc -> acc ++ List.drop (List.length row - 1) row)
                    []
                    listOfRowVectorsRREFStar
        in
        Inconsistant "No Unique Solution: Least Squares Solution Provided"
            (UniqueSolution
                (solutionStar
                    |> Vector.Vector
                    |> ColumnVector.ColumnVector
                )
            )

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
                |> ColumnVector.ColumnVector
            )
            |> Consistant


{-| Predicate to determine if a list of Vectors are linearly independent
-}
areLinearlyIndependent : RowVector.InnerProductSpace a -> List (ColumnVector.ColumnVector a) -> Bool
areLinearlyIndependent innerProductSpace columnVectors =
    let
        matrix =
            createMatrixFromColumnVectors columnVectors
    in
    rank innerProductSpace matrix == nDimension matrix


{-| Determine whether list of vectors spans a space
-}
doesSetSpanSpace : RowVector.VectorSpace a -> VectorDimension -> List (ColumnVector.ColumnVector a) -> Result String Bool
doesSetSpanSpace vSpace (VectorDimension vectorDimension) columnVectors =
    if List.length columnVectors /= vectorDimension then
        Err "Please input same number of vectors as vector space"

    else if not <| List.all (\columnVector -> ColumnVector.dimension columnVector == vectorDimension) columnVectors then
        Err "Please input vectors of equal length as vector space"

    else
        identity vSpace.field vectorDimension
            == gaussJordan vSpace (createMatrixFromColumnVectors columnVectors)
            |> Ok


{-| Number of columns in Matrix
-}
nDimension : Matrix a -> Int
nDimension (Matrix listOfRowVectors) =
    case listOfRowVectors of
        [] ->
            0

        x :: _ ->
            RowVector.dimension x


{-| Number of rows in Matrix
-}
mDimension : Matrix a -> Int
mDimension (Matrix listOfRowVectors) =
    List.length listOfRowVectors


{-| Determine whether list of vectors are a basis for a space
-}
areBasis : RowVector.InnerProductSpace a -> VectorDimension -> List (ColumnVector.ColumnVector a) -> Bool
areBasis innerProductSpace vectorDimension vectors =
    doesSetSpanSpace innerProductSpace.vectorSpace vectorDimension vectors
        == Ok True
        && areLinearlyIndependent innerProductSpace vectors


{-| Left fold over a Matrix
-}
foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl foldFunction acc (Matrix listOfRowVectors) =
    List.foldl
        (\row accumlator -> RowVector.foldl foldFunction accumlator row)
        acc
        listOfRowVectors


{-| Monoid empty for Matrix
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
            RowVector.append
            listOne
            listTwo
            |> Matrix

    else if difference > 0 then
        List.map2
            RowVector.append
            listOne
            (listTwo ++ List.repeat difference RowVector.empty)
            |> Matrix

    else
        List.map2
            RowVector.append
            (listOne ++ List.repeat (Basics.abs difference) RowVector.empty)
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
equal : (a -> a -> Bool) -> Typeclasses.Classes.Equality.Equality (Matrix a)
equal comparator =
    Typeclasses.Classes.Equality.eq (equalImplementation comparator)


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> Matrix a -> Maybe a
getAt ( rowIndex, columnIndex ) (Matrix listOfRowVectors) =
    List.Extra.getAt rowIndex listOfRowVectors
        |> Maybe.andThen (RowVector.getAt columnIndex)


{-| Set the value in a Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> a -> Matrix a -> Matrix a
setAt ( rowIndex, columnIndex ) element (Matrix listOfRowVectors) =
    List.Extra.getAt rowIndex listOfRowVectors
        |> Maybe.map (RowVector.setAt columnIndex element)
        |> Maybe.map
            (\newRow ->
                List.Extra.setAt rowIndex newRow listOfRowVectors
            )
        |> Maybe.withDefault listOfRowVectors
        |> Matrix


{-| Calculate the commuter of two Hermitian Matricies
-}
commuter : RowVector.InnerProductSpace a -> Matrix a -> Matrix a -> Result String (Matrix a)
commuter innerProductSpace matrixOne matrixTwo =
    Result.map2 (subtract innerProductSpace.vectorSpace.field)
        (multiplyIfCan innerProductSpace matrixOne matrixTwo)
        (multiplyIfCan innerProductSpace matrixTwo matrixOne)


{-| Print a Real matrix to a string
-}
printRealMatrix : Matrix (Real.Real Float) -> String
printRealMatrix (Matrix listOfRowVectors) =
    "Matrix [ "
        ++ RowVector.printRealRowVectorList listOfRowVectors
        ++ " ]"


{-| Print a Complex matrix to a string
-}
printComplexMatrix : Matrix (ComplexNumbers.ComplexNumber Float) -> String
printComplexMatrix (Matrix listOfRowVectors) =
    "Matrix [ "
        ++ RowVector.printComplexRowVectorList listOfRowVectors
        ++ " ]"


{-| Try to read a string into a Matrix
-}
readRealMatrix : String -> Result (List Parser.DeadEnd) (Matrix (Real.Real Float))
readRealMatrix matrixString =
    Parser.run (parseMatrix Real.parseReal) matrixString


{-| Try to read a string into a Matrix
-}
readComplexMatrix : String -> Result (List Parser.DeadEnd) (Matrix (ComplexNumbers.ComplexNumber Float))
readComplexMatrix matrixString =
    Parser.run (parseMatrix ComplexNumbers.parseComplexNumber) matrixString


listOfRowVectorParser : Parser.Parser (RowVector.RowVector a) -> Parser.Parser (List (RowVector.RowVector a))
listOfRowVectorParser rowVectorParser =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = rowVectorParser
        , trailing = Parser.Forbidden
        }


parseMatrix : Parser.Parser a -> Parser.Parser (Matrix a)
parseMatrix matrixElementParser =
    Parser.succeed Matrix
        |. Parser.keyword "Matrix"
        |. Parser.spaces
        |= listOfRowVectorParser (RowVector.parseRowVector matrixElementParser)
