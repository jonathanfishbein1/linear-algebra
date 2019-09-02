module MatrixTests.MatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test "tests matrix jordanReduce put matrix into Reduced Row Echelon Form" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , Matrix.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.gaussJordan matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 0.0, 0.0, -8.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, 0.0, 1.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        , Test.test "tests matrix gaussJordan produces correct answers" <|
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
                        Matrix.solve matrix b

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ -8.0, 1.0, -2.0 ]
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.UniqueSolution expected)
        , Test.test "tests matrix gaussJordan produces correct answers second example" <|
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
                        Matrix.solve matrix b

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ 5, -1.0, -1.0 ]
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.UniqueSolution expected)
        , Test.test "tests matrix gaussJordan with no solutions" <|
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
                        Matrix.solve matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.NoUniqueSolution "No Unique Solution")
        , Test.test "tests matrix gaussJordan with infinite solutions" <|
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
                        Matrix.solve matrix b
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.InfiniteSolutions { nullity = 3, rank = 2 })
        , Test.test "tests matrix null space calculation" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, -3 ]
                            ]

                    nullSpace =
                        Matrix.nullSpace matrix

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ 0, 0 ]
                in
                Expect.equal nullSpace (Matrix.UniqueSolution expected)
        , Test.test "tests matrix linearlyIndependent" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 2 ]
                        , Vector.Vector [ 0, -3 ]
                        ]
                in
                Expect.true "Two vectors are linearly independent" (Matrix.areLinearlyIndependent listOfVectors)
        , Test.test "tests matrix linearlyIndependent with two colinear vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 2 ]
                        , Vector.Vector [ 2, 4 ]
                        ]
                in
                Expect.false "Two vectors are linearly dependent" (Matrix.areLinearlyIndependent listOfVectors)
        , Test.test "tests matrix doesSetSpanSpace with standard basis vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfVectors
                in
                Expect.equal result (Ok True)
        , Test.test "tests matrix doesSetSpanSpace with zero vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfVectors
                in
                Expect.equal result (Ok False)
        , Test.test "tests matrix doesSetSpanSpace with zero vectors 3 dimensions" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 0, 1, 0 ]
                        , Vector.Vector [ 0, 0, 1 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.doesSetSpanSpace r3 listOfVectors
                in
                Expect.equal result (Ok True)
        , Test.test "tests matrix doesSetSpanSpace with three vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 1, 0, 0 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.doesSetSpanSpace r3 listOfVectors
                in
                Expect.equal result (Ok False)
        , Test.test "tests matrix doesSetSpanSpace with three vectors testing r2" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfVectors
                in
                Expect.equal result (Err "Please input same number of vectors as vector space")
        , Test.test "tests matrix doesSetSpanSpace with three dimensional vector against R2" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 2, 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfVectors
                in
                Expect.equal result (Err "Please input vectors of equal length as vector space")
        , Test.test "tests matrix areBasis with standard basis vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfVectors
                in
                Expect.true "Vectors are basis for R2" result
        , Test.test "tests matrix areBasis with zero vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfVectors
                in
                Expect.false "Vectors are not basis for R2" result
        , Test.test "tests matrix areBasis with standard basis vectors 3 dimensions" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.areBasis r3 listOfVectors
                in
                Expect.false "Vectors are not basis for R3" result
        , Test.test "tests matrix areBasis with zero vectors 3 dimensions" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0, 0 ]
                        , Vector.Vector [ 0, 1, 0 ]
                        , Vector.Vector [ 0, 0, 1 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.areBasis r3 listOfVectors
                in
                Expect.true "Vectors are basis fro R3" result
        , Test.test "tests matrix areBasis with three vectors" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.areBasis r3 listOfVectors
                in
                Expect.false "Vectors are not basis R3" result
        , Test.test "tests matrix areBasis with three vectors testing r2" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 1, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfVectors
                in
                Expect.false "Vectors are not basis for R2" result
        , Test.test "tests matrix areBasis with three dimensional vector against R2" <|
            \_ ->
                let
                    listOfVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 2, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfVectors
                in
                Expect.false "Vectos are not basis for R2" result
        , Test.test "tests matrix nullity with infinite solutions" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 1, 2, 3, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 1, 3, 1, 4 ]
                            ]
                in
                Expect.equal (Matrix.solveMatrix matrix) (Matrix.InfiniteSolutions { nullity = 3, rank = 2 })
        , Test.test "tests basisOfVectorSpace returns R2 basis vectors" <|
            \_ ->
                let
                    r2BasisVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    testbasisVectors =
                        Matrix.basisOfVectorSpace r2 r2BasisVectors
                in
                Expect.equal r2BasisVectors testbasisVectors
        , Test.test "tests basisOfVectorSpace returns R2 basis vectors for non basis vectors" <|
            \_ ->
                let
                    r2BasisVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
                        ]

                    testVectors =
                        [ Vector.Vector [ 1, 0 ]
                        , Vector.Vector [ 0, 1 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    testbasisVectors =
                        Matrix.basisOfVectorSpace r2 testVectors
                in
                Expect.equal r2BasisVectors testbasisVectors
        , Test.test "tests matrix fold" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 1 ]
                            ]

                    result =
                        Matrix.VectorSpace 2

                    foldResult =
                        Matrix.foldl (+) 0 matrix
                in
                Expect.equal foldResult 2
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "getAt index" <|
            \one two three ->
                let
                    matrix =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ one, two, three ] ]
                in
                Expect.equal (Matrix.getAt ( 0, 0 ) matrix) (Just one)
        , Test.fuzz Fuzz.int "setAt getAt index" <|
            \one ->
                let
                    matrix =
                        Matrix.setAt ( 0, 0 ) one (Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ 0 ] ])
                in
                Expect.equal (Matrix.getAt ( 0, 0 ) matrix) (Just one)
        , Test.test "print Matrix" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ 0, 1 ] ]

                    printedMatrix =
                        Matrix.print matrix
                in
                Expect.equal printedMatrix "Matrix [ RowVector Vector [0, 1] ] ]"
        , Test.fuzz2 Fuzz.float Fuzz.float "read Matrix" <|
            \one two ->
                let
                    matrix =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one, two ] ]

                    printedMatrix =
                        Matrix.print matrix

                    readMatrix =
                        Matrix.read printedMatrix
                in
                Expect.equal readMatrix (Ok matrix)
        , Test.test "tests matrix determinant 2 x 2" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                            , Matrix.RowVector <| Vector.Vector [ 3, 4 ]
                            ]

                    determinant =
                        Matrix.determinant matrix
                in
                Expect.equal determinant (Ok -2)
        , Test.test "tests matrix determinant 3 x 3" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, -1, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 0, 1 ]
                            ]

                    determinant =
                        Matrix.determinant matrix
                in
                Expect.equal determinant (Ok 35)
        , Test.test "tests matrix determinant 4 x 4" <|
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
                        Matrix.determinant matrix
                in
                Expect.equal determinant (Ok 7)
        , Test.test "tests matrix invert" <|
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
                        Matrix.invert matrix
                in
                Expect.equal inverse (Ok expectedInverse)
        , Test.test "tests matrix times inverse equals identity" <|
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

                    identityMatrix =
                        Matrix.identityMatrix (Matrix.mDimension matrix)

                    matrixInverseProduct =
                        Matrix.multiplyRealMatrices matrix inverse
                in
                Expect.equal matrixInverseProduct (Ok identityMatrix)
        , Test.test "tests inverse times matrix equals identity" <|
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

                    identityMatrix =
                        Matrix.identityMatrix (Matrix.mDimension matrix)

                    inverseMatrixProduct =
                        Matrix.multiplyRealMatrices inverse matrix
                in
                Expect.equal inverseMatrixProduct (Ok identityMatrix)
        , Test.test "tests complex matrix determinant 2 x 2" <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
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
                        Matrix.determinantComplex matrix

                    expectedDeterminant =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                -2
                            )
                            (ComplexNumbers.Imaginary
                                4
                            )
                in
                Expect.equal determinantComplex (Ok expectedDeterminant)
        , Test.test "tests complex matrix inverse 2 x 2" <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                1
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                2
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                3
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
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
                        Matrix.invertComplex matrix

                    expectedComplexNumberR1C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                -(2 / 5)
                            )
                            (ComplexNumbers.Imaginary
                                -(4 / 5)
                            )

                    expectedComplexNumberR1C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (1 / 5)
                            )
                            (ComplexNumbers.Imaginary
                                (2 / 5)
                            )

                    expectedComplexNumberR2C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (3 / 10)
                            )
                            (ComplexNumbers.Imaginary
                                (3 / 5)
                            )

                    expectedComplexNumberR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
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
        , Test.test "tests complex matrix inverse 3 x 3 is unitary" <|
            \_ ->
                let
                    complexNumberR1C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                (1 / 2)
                            )

                    complexNumberR1C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (1 / Basics.sqrt 3)
                            )

                    complexNumberR1C3 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (3 / (2 * Basics.sqrt 15))
                            )
                            (ComplexNumbers.Imaginary
                                (1 / (2 * Basics.sqrt 15))
                            )

                    complexNumberR2C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (-1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (1 / Basics.sqrt 3)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR2C3 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (4 / (2 * Basics.sqrt 15))
                            )
                            (ComplexNumbers.Imaginary
                                (3 / (2 * Basics.sqrt 15))
                            )

                    complexNumberR3C1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                (1 / 2)
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    complexNumberR3C2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                0
                            )
                            (ComplexNumbers.Imaginary
                                (-1 / Basics.sqrt 3)
                            )

                    complexNumberR3C3 =
                        ComplexNumbers.ComplexNumberCartesian
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
        , Test.test "tests invertability" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 2, 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 1, 3 ]
                            ]

                    isInvertable =
                        Matrix.isInvertable matrix
                in
                Expect.equal isInvertable (Err "Determinant is zero matrix is not invertable")
        , Test.test "tests sumMatrix" <|
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
        , Test.test "tests invertabilityComplex" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ]
                            , Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            ]

                    isInvertable =
                        Matrix.isInvertableComplex matrix
                in
                Expect.equal isInvertable (Err "Determinant is zero matrix is not invertable")
        , Test.fuzz Fuzz.int "test if matrix is square" <|
            \one ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, one ]
                            , Matrix.RowVector <| Vector.Vector [ one, one ]
                            ]
                in
                Expect.true "matrix is square" (Matrix.isSquareMatrix matrix)
        ]
