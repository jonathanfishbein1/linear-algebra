module MatrixTests.MatrixTests exposing (suite)

import ColumnVector
import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Matrix
import Monoid
import Real
import RowVector
import SquareMatrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test
            "tests matrix null space calculation"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, -3 ]
                            ]
                            |> Matrix.map Real.Real

                    nullSpace =
                        Matrix.nullSpace Real.equal Vector.realInnerProductSpace matrix

                    expected =
                        ColumnVector.ColumnVector <| Vector.Vector [ Real.zero, Real.zero ]
                in
                Expect.equal nullSpace (Matrix.Consistant (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix linearlyIndependent"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 2
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 0
                                , -3
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]
                in
                Expect.true "Two vectors are linearly independent" (Matrix.areLinearlyIndependent Vector.realInnerProductSpace listOfVectors)
        , Test.test
            "tests matrix linearlyIndependent with two colinear vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 2
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 2
                                , 4
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]
                in
                Expect.false "Two vectors are linearly dependent" (Matrix.areLinearlyIndependent Vector.realInnerProductSpace listOfVectors)
        , Test.test "tests matrix doesSetSpanSpace with standard basis vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 0
                                , 1
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.doesSetSpanSpace Vector.realVectorSpace r2 listOfVectors
                in
                Expect.equal result (Ok True)
        , Test.test
            "tests matrix doesSetSpanSpace with zero vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.zeros Monoid.numberSum 2)
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.zeros Monoid.numberSum 2)
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.doesSetSpanSpace Vector.realVectorSpace r2 listOfVectors
                in
                Expect.equal result (Ok False)
        , Test.test
            "tests matrix doesSetSpanSpace with identity matrix 3 dimensions"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 0, 1, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 0, 0, 1 ])
                            |> ColumnVector.map Real.Real
                        ]

                    r3 =
                        Matrix.VectorDimension 3

                    result =
                        Matrix.doesSetSpanSpace Vector.realVectorSpace r3 listOfVectors
                in
                Expect.equal result (Ok True)
        , Test.test
            "tests matrix doesSetSpanSpace with three vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0, 0 ])
                            |> ColumnVector.map Real.Real
                        ]

                    r3 =
                        Matrix.VectorDimension 3

                    result =
                        Matrix.doesSetSpanSpace Vector.realVectorSpace r3 listOfVectors
                in
                Expect.equal result (Ok False)
        , Test.test
            "tests matrix doesSetSpanSpace with three vectors testing r2"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0 ])
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.doesSetSpanSpace Vector.realVectorSpace r2 listOfVectors
                in
                Expect.equal result (Err "Please input same number of vectors as vector space")
        , Test.test
            "tests matrix doesSetSpanSpace with three dimensional vector against R2"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector [ 1, 0, 0 ])
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector [ 2, 0, 0 ])
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.doesSetSpanSpace Vector.realVectorSpace r2 listOfVectors
                in
                Expect.equal result (Err "Please input vectors of equal length as vector space")
        , Test.test
            "tests matrix areBasis with standard basis vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 0
                                , 1
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r2 listOfVectors
                in
                Expect.true "Vectors are basis for R2" result
        , Test.test
            "tests matrix areBasis with zero vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.zeros Monoid.numberSum 2)
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.zeros Monoid.numberSum 2)
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r2 listOfVectors
                in
                Expect.false "Vectors are not basis for R2" result
        , Test.test
            "tests matrix areBasis with standard basis vectors 3 dimensions"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r3 =
                        Matrix.VectorDimension 3

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r3 listOfVectors
                in
                Expect.false "Vectors are not basis for R3" result
        , Test.test
            "tests matrix areBasis with identity matrix 3 dimensions"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 0
                                , 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 0
                                , 0
                                , 1
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r3 =
                        Matrix.VectorDimension 3

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r3 listOfVectors
                in
                Expect.true "Vectors are basis fro R3" result
        , Test.test
            "tests matrix areBasis with three vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r3 =
                        Matrix.VectorDimension 3

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r3 listOfVectors
                in
                Expect.false "Vectors are not basis R3" result
        , Test.test
            "tests matrix areBasis with three vectors testing r2"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r2 listOfVectors
                in
                Expect.false "Vectors are not basis for R2" result
        , Test.test
            "tests matrix areBasis with three dimensional vector against R2"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 1
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        , ColumnVector.ColumnVector
                            (Vector.Vector
                                [ 2
                                , 0
                                ]
                            )
                            |> ColumnVector.map Real.Real
                        ]

                    r2 =
                        Matrix.VectorDimension 2

                    result =
                        Matrix.areBasis Vector.realInnerProductSpace r2 listOfVectors
                in
                Expect.false "Vectos are not basis for R2" result
        , Test.test
            "tests matrix nullity with infinite solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 1, 1, 3, 1 ]
                            ]
                            |> Matrix.map Real.Real

                    b =
                        Vector.Vector [ 2, 4 ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                in
                Expect.equal (Matrix.solve Real.equal Vector.realInnerProductSpace matrix b)
                    (Matrix.Consistant (Matrix.InfiniteSolutions { nullity = 3, rank = 2 }))
        , Test.test
            "tests matrix fold"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 0 ]
                            , RowVector.RowVector <| Vector.Vector [ 0, 1 ]
                            ]

                    foldResult =
                        Matrix.foldl (+) 0 matrix
                in
                Expect.equal foldResult 2
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "getAt index"
          <|
            \one two three ->
                let
                    matrix =
                        Matrix.Matrix [ RowVector.RowVector <| Vector.Vector [ one, two, three ] ]
                in
                Expect.equal (Matrix.getAt ( 0, 0 ) matrix) (Just one)
        , Test.fuzz
            Fuzz.int
            "setAt getAt index"
          <|
            \one ->
                let
                    matrix =
                        Matrix.setAt ( 0, 0 ) one (Matrix.Matrix <| [ RowVector.RowVector <| Vector.Vector [ 0 ] ])
                in
                Expect.equal (Matrix.getAt ( 0, 0 ) matrix) (Just one)
        , Test.test
            "print Matrix"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix <| [ RowVector.RowVector <| Vector.Vector [ 0, 1 ] ]

                    printedMatrix =
                        Matrix.printRealMatrix matrix
                in
                Expect.equal printedMatrix "Matrix [ RowVector Vector [0, 1] ] ]"
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "read Matrix"
          <|
            \one two ->
                let
                    matrix =
                        Matrix.Matrix <| [ RowVector.RowVector <| Vector.Vector [ one, two ] ]

                    printedMatrix =
                        Matrix.printRealMatrix matrix

                    readMatrix =
                        Matrix.readRealMatrix printedMatrix
                in
                Expect.equal readMatrix (Ok matrix)
        , Test.test
            "tests sumMatrix"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 4, 5, 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 7, 8, 9 ]
                            ]

                    subMatrix =
                        Matrix.subMatrix 1 3 1 3 matrix

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 5, 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 8, 9 ]
                            ]
                in
                Expect.equal subMatrix expected
        , Test.fuzz
            Fuzz.int
            "test if matrix is square"
          <|
            \one ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ one, one ]
                            , RowVector.RowVector <| Vector.Vector [ one, one ]
                            ]
                in
                Expect.true "matrix is square" (SquareMatrix.isSquareMatrix matrix)
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix transpose transpose is idempotent"
          <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    mTransposeTranspose =
                        Matrix.transpose m
                            |> Matrix.transpose
                in
                Expect.equal m mTransposeTranspose
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix transpose respects addition"
          <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    m2 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        two
                                        (Imaginary.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Transpose =
                        Matrix.add ComplexNumbers.field m1 m2
                            |> Matrix.transpose

                    m1TransposePlusm2Transpose =
                        Matrix.transpose m1
                            |> Matrix.add ComplexNumbers.field (Matrix.transpose m2)
                in
                Expect.equal m1Plusm2Transpose m1TransposePlusm2Transpose
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix transpose respects scalar multiplication"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    m1 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenTranspose =
                        Matrix.scalarMultiplication ComplexNumbers.field c m1
                            |> Matrix.transpose

                    cTransposeOfA =
                        Matrix.transpose m1
                            |> Matrix.scalarMultiplication ComplexNumbers.field c
                in
                Expect.equal cAThenTranspose cTransposeOfA
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix conjugate conjugate is idempotent"
          <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    mConjugateConjugate =
                        Matrix.conjugate m
                            |> Matrix.conjugate
                in
                Expect.equal m mConjugateConjugate
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix conjugate respects addition"
          <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    m2 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        two
                                        (Imaginary.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Conjugate =
                        Matrix.add ComplexNumbers.field m1 m2
                            |> Matrix.conjugate

                    m1ConjugatePlusm2Conjugate =
                        Matrix.conjugate m1
                            |> Matrix.add ComplexNumbers.field (Matrix.conjugate m2)
                in
                Expect.equal m1Plusm2Conjugate m1ConjugatePlusm2Conjugate
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix conjugate respects scalar multiplication"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    cConjugate =
                        ComplexNumbers.conjugate c

                    m1 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenConjugate =
                        Matrix.scalarMultiplication ComplexNumbers.field c m1
                            |> Matrix.conjugate

                    cConjugateOfA =
                        Matrix.conjugate m1
                            |> Matrix.scalarMultiplication ComplexNumbers.field cConjugate
                in
                Expect.equal cAThenConjugate cConjugateOfA
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix adjoint is idempotent"
          <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    mAdjoint =
                        Matrix.adjoint m
                            |> Matrix.adjoint
                in
                Expect.equal m mAdjoint
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix adjoint respects addition"
          <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    m2 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        two
                                        (Imaginary.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Adjoint =
                        Matrix.add ComplexNumbers.field m1 m2
                            |> Matrix.adjoint

                    m1ConjugatePlusm2Adjoint =
                        Matrix.adjoint m1
                            |> Matrix.add ComplexNumbers.field (Matrix.conjugate m2)
                in
                Expect.equal m1Plusm2Adjoint m1ConjugatePlusm2Adjoint
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix adjoint respects scalar multiplication"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    cConjugate =
                        ComplexNumbers.conjugate c

                    m1 =
                        Matrix.Matrix
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenAdjoint =
                        Matrix.scalarMultiplication ComplexNumbers.field c m1
                            |> Matrix.adjoint

                    cAdjointOfA =
                        Matrix.adjoint m1
                            |> Matrix.scalarMultiplication ComplexNumbers.field cConjugate
                in
                Expect.equal cAThenAdjoint cAdjointOfA
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests real Matrix multiplication respects scalar multiplication"
          <|
            \one two three ->
                let
                    v1 =
                        Vector.Vector
                            [ three
                            , one
                            , three
                            , two
                            ]
                            |> RowVector.RowVector

                    v2 =
                        Vector.Vector
                            [ one
                            , three
                            , three
                            , two
                            ]
                            |> RowVector.RowVector

                    m1 =
                        Matrix.Matrix
                            [ v1 ]

                    m2 =
                        Matrix.Matrix [ v2 ]

                    cTimesm1Timem2 =
                        Matrix.multiply Vector.realInnerProductSpace m1 m2
                            |> Result.map (Matrix.scalarMultiplication Real.field one)

                    cTimesm1ThenTimesm2 =
                        Matrix.multiply Vector.realInnerProductSpace (Matrix.scalarMultiplication Real.field one m1) m2
                in
                Expect.equal cTimesm1Timem2 cTimesm1ThenTimesm2
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix tensor product respects addition"
          <|
            \one two three ->
                let
                    matrixI =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ one, two ]
                            , RowVector.RowVector <| Vector.Vector [ one, two ]
                            ]

                    matrixJ =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ three, one ]
                            , RowVector.RowVector <| Vector.Vector [ one, two ]
                            ]

                    matrixK =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <| Vector.Vector [ two, three ]
                            , RowVector.RowVector <| Vector.Vector [ one, two ]
                            ]

                    matrixSumIJ =
                        Matrix.add Real.field matrixI matrixJ

                    tensorProductIJK =
                        Matrix.tensorProduct Real.field matrixSumIJ matrixK

                    tensorProductIK =
                        Matrix.tensorProduct Real.field matrixI matrixK

                    tensorProductJK =
                        Matrix.tensorProduct Real.field matrixJ matrixK

                    matrixSumTensorProductIKJK =
                        Matrix.add Real.field tensorProductIK tensorProductJK
                in
                Expect.true "matricies equal" ((Matrix.equal Real.equal.eq).eq tensorProductIJK matrixSumTensorProductIKJK)
        , Test.test
            "tests areRowEquivalent"
          <|
            \_ ->
                let
                    matrixOne =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> Matrix.map Real.Real

                    matrixTwo =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> Matrix.map Real.Real
                            |> Matrix.scalarMultiplication Real.field (Real.Real 2)
                in
                Expect.true "Are row equivalent" (Matrix.areRowEquivalent Vector.realVectorSpace matrixOne matrixTwo)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix rank"
          <|
            \one two three ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ one, Real.zero, Real.zero ]
                            , RowVector.RowVector <| Vector.Vector [ Real.zero, two, Real.zero ]
                            , RowVector.RowVector <| Vector.Vector [ Real.zero, Real.zero, three ]
                            ]

                    rank =
                        Matrix.rank Vector.realInnerProductSpace matrix
                in
                Expect.equal rank 3
        , Test.fuzz2
            (Fuzz.map Basics.toFloat (Fuzz.intRange 1 10))
            (Fuzz.map Basics.toFloat (Fuzz.intRange 1 10))
            "tests matrix rank with two colinear vectors"
          <|
            \one two ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ one, two ]
                            , RowVector.RowVector <| Vector.Vector [ one * 2, two * 2 ]
                            ]
                            |> Matrix.map Real.Real
                in
                Expect.equal (Matrix.rank Vector.realInnerProductSpace matrix) 1
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests matrix createMatrixFromColumnVectors"
          <|
            \one two ->
                let
                    vectorOne =
                        Vector.Vector [ one, one ]

                    veectorTwo =
                        Vector.Vector [ two, two ]

                    matrix =
                        Matrix.createMatrixFromColumnVectors [ ColumnVector.ColumnVector vectorOne, ColumnVector.ColumnVector veectorTwo ]

                    expectedMatrix =
                        Matrix.Matrix
                            [ RowVector.RowVector vectorOne
                            , RowVector.RowVector veectorTwo
                            ]
                            |> Matrix.transpose
                in
                Expect.equal matrix expectedMatrix
        ]
