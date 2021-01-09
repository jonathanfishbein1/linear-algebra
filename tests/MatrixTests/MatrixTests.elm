module MatrixTests.MatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Float.Extra
import Fuzz
import Matrix
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test
            "tests matrix jordanReduce put matrix into Reduced Row Echelon Form"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , Matrix.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.gaussJordan Vector.realVectorSpace matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, -8.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, 1.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test
            "tests matrix gaussJordan produces correct answers"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -2, 0, -3 ]
                            ]

                    b =
                        Matrix.ColumnVector <| Vector.Vector [ -4, -11, 22 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Vector.realInnerProductSpace matrix b

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ -8.0, 1.0, -2.0 ]
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.Consistant (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix gaussJordan produces correct answers second example"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 1, 1 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 3, 4 ]
                            ]

                    b =
                        Matrix.ColumnVector <| Vector.Vector [ 3, 0, -2 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Vector.realInnerProductSpace matrix b

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ 5, -1.0, -1.0 ]
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.Consistant (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix gaussJordan with no solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 2, 2, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 4, 0, 6 ]
                            ]

                    b =
                        Matrix.ColumnVector <| Vector.Vector [ 8, 12, 4 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Vector.realInnerProductSpace matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.Inconsistant "No Unique Solution")
        , Test.test
            "tests matrix gaussJordan with infinite solutions"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 1, 1 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 2, 2, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 4, 0, 6 ]
                            ]

                    b =
                        Matrix.ColumnVector <| Vector.Vector [ 7, 12, 4 ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve Vector.realInnerProductSpace matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix
                    (Matrix.Consistant (Matrix.InfiniteSolutions { nullity = 3, rank = 2 }))
        , Test.test
            "tests matrix null space calculation"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, -3 ]
                            ]

                    nullSpace =
                        Matrix.nullSpace Vector.realInnerProductSpace matrix

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ 0, 0 ]
                in
                Expect.equal nullSpace (Matrix.Consistant (Matrix.UniqueSolution expected))
        , Test.test
            "tests matrix linearlyIndependent"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 2 ]
                        , Vector.Vector [ 0, -3 ]
                        ]
                in
                Expect.true "Two vectors are linearly independent" (Matrix.areLinearlyIndependent Vector.realInnerProductSpace listOfVectors)
        , Test.test
            "tests matrix linearlyIndependent with two colinear vectors"
          <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 2 ]
                        , Vector.Vector [ 2, 4 ]
                        ]
                in
                Expect.false "Two vectors are linearly dependent" (Matrix.areLinearlyIndependent Vector.realInnerProductSpace listOfVectors)
        , Test.test "tests matrix doesSetSpanSpace with standard basis vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
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
                        [ Vector.zeros Monoid.numberSum 2
                        , Vector.zeros Monoid.numberSum 2
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
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 0, 1, 0 ]
                        , Vector.Vector [ 0, 0, 1 ]
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
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 1, 0, 0 ]
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
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
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
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 2, 0, 0 ]
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
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
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
                        [ Vector.zeros Monoid.numberSum 2
                        , Vector.zeros Monoid.numberSum 2
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
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
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
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 0, 1, 0 ]
                        , Vector.Vector [ 0, 0, 1 ]
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
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
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
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
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
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 2, 0 ]
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
                            [ Matrix.RowVector <| Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 3, 1, 4 ]
                            ]
                in
                Expect.equal (Matrix.solveMatrix Vector.realInnerProductSpace matrix)
                    (Matrix.Consistant (Matrix.InfiniteSolutions { nullity = 3, rank = 2 }))
        , Test.test
            "tests matrix fold"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 1 ]
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
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ one, two, three ] ]
                in
                Expect.equal (Matrix.getAt ( 0, 0 ) matrix) (Just one)
        , Test.fuzz
            Fuzz.int
            "setAt getAt index"
          <|
            \one ->
                let
                    matrix =
                        Matrix.setAt ( 0, 0 ) one (Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ 0 ] ])
                in
                Expect.equal (Matrix.getAt ( 0, 0 ) matrix) (Just one)
        , Test.test
            "print Matrix"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ 0, 1 ] ]

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
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one, two ] ]

                    printedMatrix =
                        Matrix.printRealMatrix matrix

                    readMatrix =
                        Matrix.readRealMatrix printedMatrix
                in
                Expect.equal readMatrix (Ok matrix)
        , Test.test
            "tests matrix determinant 2 x 2"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 3, 4 ]
                            ]

                    determinant =
                        Matrix.determinant Vector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok -2)
        , Test.test
            "tests matrix determinant 3 x 3"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, -1, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 0, 1 ]
                            ]

                    determinant =
                        Matrix.determinant Vector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok 35)
        , Test.test
            "tests matrix determinant 4 x 4"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 3, 4 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 0, 2, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, 0, 0 ]
                            ]

                    determinant =
                        Matrix.determinant Vector.realVectorSpace matrix
                in
                Expect.equal determinant (Ok 7)
        , Test.test
            "tests matrix invert"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]

                    expectedInverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]

                    inverse =
                        Matrix.invert Vector.realInnerProductSpace matrix
                in
                Expect.equal inverse (Ok expectedInverse)
        , Test.test
            "tests matrix times inverse equals identity"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]

                    inverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]

                    identity =
                        Matrix.identity Field.float (Matrix.mDimension matrix)

                    matrixInverseProduct =
                        Matrix.multiply Vector.realInnerProductSpace matrix inverse
                in
                Expect.equal matrixInverseProduct (Ok identity)
        , Test.test
            "tests inverse times matrix equals identity"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, -1, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ -1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 4 ]
                            ]

                    inverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 3, -1 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 5, -2 ]
                            , Matrix.RowVector <| Vector.Vector [ -3, -2, 1 ]
                            ]

                    identity =
                        Matrix.identity Field.float (Matrix.mDimension matrix)

                    inverseMatrixProduct =
                        Matrix.multiply Vector.realInnerProductSpace inverse matrix
                in
                Expect.equal inverseMatrixProduct (Ok identity)
        , Test.test
            "tests complex matrix determinant 2 x 2"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                4
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    determinantComplex =
                        Matrix.determinant Vector.complexVectorSpace matrix

                    expectedDeterminant =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                -2
                            )
                            (ComplexNumbers.Imaginary
                                4
                            )
                in
                case determinantComplex of
                    Ok dComplex ->
                        Expect.true "determinants are equal" (ComplexNumbers.equal dComplex expectedDeterminant)

                    _ ->
                        Expect.fail "determinants not equal"
        , Test.test
            "tests complex matrix inverse 2 x 2"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                4
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2 ]
                            ]

                    inverseComplex =
                        Matrix.invert Vector.complexInnerProductSpace matrix

                    expectedComplexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                -(2 / 5)
                            )
                            (ComplexNumbers.Imaginary
                                -(4 / 5)
                            )

                    expectedComplexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 5)
                            )
                            (ComplexNumbers.Imaginary
                                (2 / 5)
                            )

                    expectedComplexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (3 / 10)
                            )
                            (ComplexNumbers.Imaginary
                                (3 / 5)
                            )

                    expectedComplexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 10)
                            )
                            (ComplexNumbers.Imaginary
                                -(3 / 10)
                            )

                    expectedInverse =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ expectedComplexNumberR1C1, expectedComplexNumberR1C2 ]
                            , Matrix.RowVector <| Vector.Vector [ expectedComplexNumberR2C1, expectedComplexNumberR2C2 ]
                            ]
                in
                case inverseComplex of
                    Ok result ->
                        Expect.true "matrices are equal" (Matrix.equal ComplexNumbers.equal result expectedInverse)

                    Err error ->
                        Expect.fail error
        , Test.test
            "tests complex matrix inverse 3 x 3 is unitary"
          <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                (1 / 2)
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (1 / Basics.sqrt 3)
                            )

                    complexNumberR1C3 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (3 / (2 * Basics.sqrt 15))
                            )
                            (ComplexNumbers.Imaginary
                                (1 / (2 * Basics.sqrt 15))
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (-1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / Basics.sqrt 3)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C3 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (4 / (2 * Basics.sqrt 15))
                            )
                            (ComplexNumbers.Imaginary
                                (3 / (2 * Basics.sqrt 15))
                            )

                    complexNumberR3C1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                (1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR3C2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (-1 / Basics.sqrt 3)
                            )

                    complexNumberR3C3 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (5 / (2 * Basics.sqrt 15))
                            )

                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ complexNumberR1C1, complexNumberR1C2, complexNumberR1C3 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR2C1, complexNumberR2C2, complexNumberR2C3 ]
                            , Matrix.RowVector <| Vector.Vector [ complexNumberR3C1, complexNumberR3C2, complexNumberR3C3 ]
                            ]

                    isUnitary =
                        Matrix.isUnitary matrix
                in
                Expect.true "is Unitary" isUnitary
        , Test.test
            "tests invertability"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 2, 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 3 ]
                            ]

                    isInvertable =
                        Matrix.isInvertable Vector.realInnerProductSpace matrix
                in
                Expect.equal isInvertable (Err "Matrix not onto Matrix is not invertable")
        , Test.test
            "tests sumMatrix"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 5, 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 8, 9 ]
                            ]

                    subMatrix =
                        Matrix.subMatrix 1 3 1 3 matrix

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 5, 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 8, 9 ]
                            ]
                in
                Expect.equal subMatrix expected
        , Test.test
            "tests invertabilityComplex"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            ]

                    isInvertable =
                        Matrix.isInvertable Vector.complexInnerProductSpace matrix
                in
                Expect.equal isInvertable (Err "Matrix not onto Matrix is not invertable")
        , Test.fuzz
            Fuzz.int
            "test if matrix is square"
          <|
            \one ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, one ]
                            , Matrix.RowVector <| Vector.Vector [ one, one ]
                            ]
                in
                Expect.true "matrix is square" (Matrix.isSquareMatrix matrix)
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix transpose transpose is idempotent"
          <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
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
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix transpose respects addition"
          <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    m2 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Transpose =
                        Matrix.add ComplexNumbers.complexField m1 m2
                            |> Matrix.transpose

                    m1TransposePlusm2Transpose =
                        Matrix.transpose m1
                            |> Matrix.add ComplexNumbers.complexField (Matrix.transpose m2)
                in
                Expect.equal m1Plusm2Transpose m1TransposePlusm2Transpose
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix transpose respects scalar multiplication"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenTranspose =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c m1
                            |> Matrix.transpose

                    cTransposeOfA =
                        Matrix.transpose m1
                            |> Matrix.scalarMultiplication ComplexNumbers.complexField c
                in
                Expect.equal cAThenTranspose cTransposeOfA
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix conjugate conjugate is idempotent"
          <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
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
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix conjugate respects addition"
          <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    m2 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Conjugate =
                        Matrix.add ComplexNumbers.complexField m1 m2
                            |> Matrix.conjugate

                    m1ConjugatePlusm2Conjugate =
                        Matrix.conjugate m1
                            |> Matrix.add ComplexNumbers.complexField (Matrix.conjugate m2)
                in
                Expect.equal m1Plusm2Conjugate m1ConjugatePlusm2Conjugate
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix conjugate respects scalar multiplication"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    cConjugate =
                        ComplexNumbers.conjugate c

                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenConjugate =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c m1
                            |> Matrix.conjugate

                    cConjugateOfA =
                        Matrix.conjugate m1
                            |> Matrix.scalarMultiplication ComplexNumbers.complexField cConjugate
                in
                Expect.equal cAThenConjugate cConjugateOfA
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix adjoint is idempotent"
          <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
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
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix adjoint respects addition"
          <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    m2 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Adjoint =
                        Matrix.add ComplexNumbers.complexField m1 m2
                            |> Matrix.adjoint

                    m1ConjugatePlusm2Adjoint =
                        Matrix.adjoint m1
                            |> Matrix.add ComplexNumbers.complexField (Matrix.conjugate m2)
                in
                Expect.equal m1Plusm2Adjoint m1ConjugatePlusm2Adjoint
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix adjoint respects scalar multiplication"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    cConjugate =
                        ComplexNumbers.conjugate c

                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenAdjoint =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c m1
                            |> Matrix.adjoint

                    cAdjointOfA =
                        Matrix.adjoint m1
                            |> Matrix.scalarMultiplication ComplexNumbers.complexField cConjugate
                in
                Expect.equal cAThenAdjoint cAdjointOfA
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests real Matrix multiplication respects scalar multiplication"
          <|
            \one two three ->
                let
                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                , two
                                ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                , two
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1 ]

                    m2 =
                        Matrix.Matrix [ v2 ]

                    cTimesm1Timem2 =
                        Matrix.multiply Vector.realInnerProductSpace m1 m2
                            |> Result.map (Matrix.scalarMultiplication Field.float one)

                    cTimesm1ThenTimesm2 =
                        Matrix.multiply Vector.realInnerProductSpace (Matrix.scalarMultiplication Field.float one m1) m2
                in
                Expect.equal cTimesm1Timem2 cTimesm1ThenTimesm2
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix tensor product respects addition"
          <|
            \one two three ->
                let
                    matrixI =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ one, two ]
                            , Matrix.RowVector <| Vector.Vector [ one, two ]
                            ]

                    matrixJ =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ three, one ]
                            , Matrix.RowVector <| Vector.Vector [ one, two ]
                            ]

                    matrixK =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ two, three ]
                            , Matrix.RowVector <| Vector.Vector [ one, two ]
                            ]

                    matrixSumIJ =
                        Matrix.add Field.float matrixI matrixJ

                    tensorProductIJK =
                        Matrix.tensorProduct Field.float matrixSumIJ matrixK

                    tensorProductIK =
                        Matrix.tensorProduct Field.float matrixI matrixK

                    tensorProductJK =
                        Matrix.tensorProduct Field.float matrixJ matrixK

                    matrixSumTensorProductIKJK =
                        Matrix.add Field.float tensorProductIK tensorProductJK
                in
                Expect.true "matricies equal" (Matrix.equal (\valOne valTwo -> Float.Extra.equalWithin 0.1 valOne valTwo) tensorProductIJK matrixSumTensorProductIKJK)
        , Test.test
            "tests right stochastic"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]

                    isRightStochastic =
                        Matrix.isRightStochastic matrix
                in
                Expect.true "Is Right Stochastic" isRightStochastic
        , Test.test
            "tests left stochastic"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]

                    isLeftStochastic =
                        Matrix.isLeftStochastic matrix
                in
                Expect.true "Is Left Stochastic" isLeftStochastic
        , Test.test
            "tests doubly stochastic"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]

                    isDoublyStochastic =
                        Matrix.isDoublyStochastic matrix
                in
                Expect.true "Is Doubly Stochastic" isDoublyStochastic
        , Test.test
            "tests areRowEquivalent"
          <|
            \_ ->
                let
                    matrixOne =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]

                    matrixTwo =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> Matrix.scalarMultiplication Field.float 2
                in
                Expect.true "Are row equivalent" (Matrix.areRowEquivalent Vector.realVectorSpace matrixOne matrixTwo)
        , Test.fuzz3
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)
            "tests matrix rank"
          <|
            \one two three ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, 0, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, two, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 0, three ]
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
                            [ Matrix.RowVector <| Vector.Vector [ one, two ]
                            , Matrix.RowVector <| Vector.Vector [ one * 2, two * 2 ]
                            ]
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
                        Matrix.createMatrixFromColumnVectors [ Matrix.ColumnVector vectorOne, Matrix.ColumnVector veectorTwo ]

                    expectedMatrix =
                        Matrix.Matrix
                            [ Matrix.RowVector vectorOne
                            , Matrix.RowVector veectorTwo
                            ]
                            |> Matrix.transpose
                in
                Expect.equal matrix expectedMatrix
        ]
