module MatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Float.Extra
import Fuzz
import List.Extra
import Matrix
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Matrix add is commutative" <|
            \one two three ->
                let
                    v =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                , ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                ]

                    w =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        two
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        three
                                    )
                                ]

                    m1 =
                        Matrix.Matrix [ v, w ]

                    m2 =
                        Matrix.Matrix [ w, v ]
                in
                Matrix.addComplexMatrices m1 m2
                    |> Expect.equal (Matrix.addComplexMatrices m2 m1)
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Matrix add is associative" <|
            \one two three ->
                let
                    v =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                , ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                ]

                    w =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        two
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        three
                                    )
                                ]

                    x =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                ]

                    m1 =
                        Matrix.Matrix [ v, w, x ]

                    m2 =
                        Matrix.Matrix [ w, v, x ]

                    m3 =
                        Matrix.Matrix [ x, w, v ]

                    m1Plusm2AndThenPlusm3 =
                        Matrix.addComplexMatrices m1 m2
                            |> Matrix.addComplexMatrices m3

                    m2Plusm3AndThenm1 =
                        Matrix.addComplexMatrices m2 m3
                            |> Matrix.addComplexMatrices m1
                in
                m1Plusm2AndThenPlusm3
                    |> Expect.equal m2Plusm3AndThenm1
        , Test.fuzz2 Fuzz.float Fuzz.float "tests Matrix empty or identity value for sum" <|
            \one two ->
                let
                    m =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    sumEmptyComplexMatrix =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero ] ]
                in
                Monoid.append (Matrix.sumComplexMatrices sumEmptyComplexMatrix) m (Monoid.empty <| Matrix.sumComplexMatrices sumEmptyComplexMatrix)
                    |> Expect.equal m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add matricies" <|
            \one two three ->
                let
                    a =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two) ] ]

                    b =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three) ] ]

                    c =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three) ] ]

                    expected =
                        Matrix.addComplexMatrices (Matrix.addComplexMatrices a b) c

                    sumEmptyComplexMatrix =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ ComplexNumbers.zero ] ]

                    listOfMonoids =
                        [ a, b, c ]
                in
                Monoid.concat (Matrix.sumComplexMatrices sumEmptyComplexMatrix) listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz2 Fuzz.int Fuzz.int "tests matrix inverse" <|
            \one two ->
                let
                    v =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.one
                                    ]
                            ]

                    w =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.negate ComplexNumbers.one
                                    ]
                            ]

                    zero =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.zero
                                    ]
                            ]
                in
                Matrix.addComplexMatrices v w
                    |> Expect.equal zero
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scalar multiplication distributes over addition" <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    w =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    v =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    vPlusW =
                        Matrix.addComplexMatrices v w

                    cvPlusW =
                        Matrix.map (ComplexNumbers.multiply c) vPlusW

                    cW =
                        Matrix.map (ComplexNumbers.multiply c) w

                    cV =
                        Matrix.map (ComplexNumbers.multiply c) v

                    cVPluscW =
                        Matrix.addComplexMatrices cW cV

                    result =
                        Matrix.equal ComplexNumbers.equal cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix transpose transpose is idempotent" <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
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
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix transpose respects addition" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
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
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Transpose =
                        Matrix.addComplexMatrices m1 m2
                            |> Matrix.transpose

                    m1TransposePlusm2Transpose =
                        Matrix.transpose m1
                            |> Matrix.addComplexMatrices (Matrix.transpose m2)
                in
                Expect.equal m1Plusm2Transpose m1TransposePlusm2Transpose
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix transpose respects scalar multiplication" <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumberCartesian
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
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenTranspose =
                        Matrix.map (ComplexNumbers.multiply c) m1
                            |> Matrix.transpose

                    cTransposeOfA =
                        Matrix.transpose m1
                            |> Matrix.map (ComplexNumbers.multiply c)
                in
                Expect.equal cAThenTranspose cTransposeOfA
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix conjugate conjugate is idempotent" <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
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
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix conjugate respects addition" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
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
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Conjugate =
                        Matrix.addComplexMatrices m1 m2
                            |> Matrix.conjugate

                    m1ConjugatePlusm2Conjugate =
                        Matrix.conjugate m1
                            |> Matrix.addComplexMatrices (Matrix.conjugate m2)
                in
                Expect.equal m1Plusm2Conjugate m1ConjugatePlusm2Conjugate
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix conjugate respects scalar multiplication" <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumberCartesian
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
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenConjugate =
                        Matrix.map (ComplexNumbers.multiply c) m1
                            |> Matrix.conjugate

                    cConjugateOfA =
                        Matrix.conjugate m1
                            |> Matrix.map (ComplexNumbers.multiply cConjugate)
                in
                Expect.equal cAThenConjugate cConjugateOfA
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix adjoint is idempotent" <|
            \one two ->
                let
                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
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
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix adjoint respects addition" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
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
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    m1Plusm2Adjoint =
                        Matrix.addComplexMatrices m1 m2
                            |> Matrix.adjoint

                    m1ConjugatePlusm2Adjoint =
                        Matrix.adjoint m1
                            |> Matrix.addComplexMatrices (Matrix.conjugate m2)
                in
                Expect.equal m1Plusm2Adjoint m1ConjugatePlusm2Adjoint
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix adjoint respects scalar multiplication" <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumberCartesian
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
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    cAThenAdjoint =
                        Matrix.map (ComplexNumbers.multiply c) m1
                            |> Matrix.adjoint

                    cAdjointOfA =
                        Matrix.adjoint m1
                            |> Matrix.map (ComplexNumbers.multiply cConjugate)
                in
                Expect.equal cAThenAdjoint cAdjointOfA
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication is associative" <|
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

                    v3 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                , two
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1 ]

                    m2 =
                        Matrix.Matrix [ v2 ]

                    m3 =
                        Matrix.Matrix [ v3 ]

                    m1Timesm2AndThenTimesm3 =
                        Matrix.multiplyRealMatrices (Matrix.multiplyRealMatrices m1 m2) m3

                    m2Timesm3AndThenTimesm1 =
                        Matrix.multiplyRealMatrices m1 (Matrix.multiplyRealMatrices m2 m3)
                in
                Expect.equal m1Timesm2AndThenTimesm3 m2Timesm3AndThenTimesm1
        , Test.test "tests identityMatrix is an identity matrix" <|
            \_ ->
                let
                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ 1
                                , 0
                                , 0
                                ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ 0
                                , 1
                                , 0
                                ]

                    v3 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ 0
                                , 0
                                , 1
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1, v2, v3 ]
                in
                Expect.equal (Matrix.identityMatrix 3) m1
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests In*A = A" <|
            \one two three ->
                let
                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                ]

                    v3 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1, v2, v3 ]

                    m1TimeI =
                        Matrix.multiplyRealMatrices m1 (Matrix.identityMatrix 3)
                in
                Expect.equal m1TimeI m1
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests A*In = a" <|
            \one two three ->
                let
                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                ]

                    v3 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1, v2, v3 ]

                    m1TimeI =
                        Matrix.multiplyRealMatrices (Matrix.identityMatrix 3) m1
                in
                Expect.equal m1TimeI m1
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication distributes over addition" <|
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

                    v3 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                , two
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1 ]

                    m2 =
                        Matrix.Matrix [ v2 ]

                    m3 =
                        Matrix.Matrix [ v3 ]

                    m1Timesm2Plus3 =
                        Matrix.multiplyRealMatrices m1 (Matrix.addRealMatrices m2 m3)

                    m1Timesm2Plusem1Timesm3 =
                        Matrix.addRealMatrices (Matrix.multiplyRealMatrices m1 m2) (Matrix.multiplyRealMatrices m1 m3)
                in
                Expect.equal m1Timesm2Plus3 m1Timesm2Plusem1Timesm3
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication distributes over addition second test" <|
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

                    v3 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                , two
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1 ]

                    m2 =
                        Matrix.Matrix [ v2 ]

                    m3 =
                        Matrix.Matrix [ v3 ]

                    m2Plusm3Timesm1 =
                        Matrix.multiplyRealMatrices (Matrix.addRealMatrices m2 m3) m1

                    m2Timesm1Plusm3Timesm1 =
                        Matrix.addRealMatrices (Matrix.multiplyRealMatrices m2 m1) (Matrix.multiplyRealMatrices m3 m1)
                in
                Expect.equal m2Plusm3Timesm1 m2Timesm1Plusm3Timesm1
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication respects scalar multiplication" <|
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
                        Matrix.multiplyRealMatrices m1 m2
                            |> Matrix.map ((*) one)

                    cTimesm1ThenTimesm2 =
                        Matrix.multiplyRealMatrices (Matrix.map ((*) one) m1) m2
                in
                Expect.equal cTimesm1Timem2 cTimesm1ThenTimesm2
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication relates to the transpose" <|
            \one two three ->
                let
                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ three, one ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ one, two ]

                    a =
                        Matrix.Matrix [ v1 ]

                    b =
                        Matrix.Matrix [ v2 ]

                    aTimebThenTranspose =
                        Matrix.multiplyRealMatrices a b
                            |> Matrix.transpose

                    cTimesm1ThenTimesm2 =
                        Matrix.multiplyRealMatrices (Matrix.transpose b) (Matrix.transpose a)
                in
                Expect.equal aTimebThenTranspose cTimesm1ThenTimesm2
        , Test.test "tests matrix vector multiplication" <|
            \_ ->
                let
                    v =
                        Vector.Vector
                            [ 1, 2, 3 ]

                    m =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, 3 ]
                            , Matrix.RowVector <| Vector.Vector [ 4, 5, 6 ]
                            , Matrix.RowVector <| Vector.Vector [ 7, 8, 9 ]
                            ]

                    mTimesV =
                        Matrix.multiplyRealVectorRealMatrix m v

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 14 ]
                            , Matrix.RowVector <| Vector.Vector [ 32 ]
                            , Matrix.RowVector <| Vector.Vector [ 50 ]
                            ]
                in
                Expect.equal mTimesV expected
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication respects the conjugate" <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real three) (ComplexNumbers.Imaginary two)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ complexNumberThree, complexNumberOne ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ complexNumberOne, complexNumberTwo ]

                    a =
                        Matrix.Matrix [ v1 ]

                    b =
                        Matrix.Matrix [ v2 ]

                    aTimebThenConjugate =
                        Matrix.multiplyComplexMatrices a b
                            |> Matrix.conjugate

                    cTimesm1ThenTimesm2 =
                        Matrix.multiplyComplexMatrices (Matrix.conjugate a) (Matrix.conjugate b)

                    result =
                        Matrix.equal ComplexNumbers.equal aTimebThenConjugate cTimesm1ThenTimesm2
                in
                Expect.true "AB conjugate equals A conjugate time B conjugate" result
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication relates to the adjoin" <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real three) (ComplexNumbers.Imaginary two)

                    v1 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ complexNumberOne ]

                    v2 =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ complexNumberTwo ]

                    a =
                        Matrix.Matrix [ v1 ]

                    b =
                        Matrix.Matrix [ v2 ]

                    aTimebThenAdjoint =
                        Matrix.multiplyComplexMatrices a b
                            |> Matrix.adjoint

                    bAdjointTimesAAdjoint =
                        Matrix.multiplyComplexMatrices (Matrix.adjoint a) (Matrix.adjoint b)

                    result =
                        Matrix.equal ComplexNumbers.equal aTimebThenAdjoint bAdjointTimesAAdjoint
                in
                Expect.true "AB adjoint equals A conjugate time B adjoint" result
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix findPivot find row with pivot entry" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ one, two ]
                            , Matrix.RowVector <| Vector.Vector [ two, two ]
                            ]

                    pivotLocation =
                        Matrix.findPivot m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix findPivot find row with pivot entry two" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                            , Matrix.RowVector <| Vector.Vector [ one, two ]
                            , Matrix.RowVector <| Vector.Vector [ two, one ]
                            ]

                    pivotLocation =
                        Matrix.findPivot m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 2

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scale scales first element to one" <|
            \one two ->
                let
                    row =
                        Matrix.RowVector <| Vector.Vector [ one, two ]

                    (Matrix.RowVector (Vector.Vector scaledRow)) =
                        Matrix.scale 0 row

                    firstElement =
                        List.Extra.getAt 0 scaledRow
                in
                case firstElement of
                    Just element ->
                        Expect.equal element 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scale scales second element by first" <|
            \one two ->
                let
                    row =
                        Matrix.RowVector <| Vector.Vector [ one, two ]

                    (Matrix.RowVector (Vector.Vector scaledRow)) =
                        Matrix.scale 0 row

                    secondElement =
                        List.Extra.getAt 1 scaledRow
                in
                case secondElement of
                    Just element ->
                        Expect.within (Expect.Absolute 0.000000001) element (two / one)

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix subrow has zero under pivot entry" <|
            \one two ->
                let
                    currentRow =
                        Matrix.RowVector <| Vector.Vector [ one, two ]

                    nextRow =
                        Matrix.RowVector <| Vector.Vector [ two, two ]

                    (Matrix.RowVector (Vector.Vector subRow)) =
                        Matrix.subrow 0 (Matrix.scale 0 currentRow) nextRow

                    firstElementSecondRow =
                        List.Extra.getAt 0 subRow
                in
                case firstElementSecondRow of
                    Just element ->
                        Expect.within (Expect.Absolute 0.000000001) element 0

                    Nothing ->
                        Expect.fail "error"
        , Test.test "tests matrix gaussianReduce put matrix into Row Echelon Form" <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , Matrix.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    rowEchelonFormMatrix =
                        Matrix.gaussianReduce matrix

                    expected =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <| Vector.Vector [ 1.0, 2.0, -1.0, -4.0 ]
                            , Matrix.RowVector <| Vector.Vector [ 0.0, 1.0, -1.0, 3.0 ]
                            , Matrix.RowVector <| Vector.Vector [ -0.0, 0.0, 1.0, -2.0 ]
                            ]
                in
                Expect.equal rowEchelonFormMatrix expected
        , Test.test "tests matrix jordanReduce put matrix into Reduced Row Echelon Form" <|
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
                Expect.equal reducedRowEchelonFormMatrix (Matrix.InfiniteSolutions { nullity = 0, rank = 0 })
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
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, -3 ]
                        ]
                in
                Expect.true "Two vectors are linearly independent" (Matrix.areLinearlyIndependent listOfRowVectors)
        , Test.test "tests matrix linearlyIndependent with two colinear vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 2 ]
                        , Matrix.RowVector <| Vector.Vector [ 2, 4 ]
                        ]
                in
                Expect.false "Two vectors are linearly dependent" (Matrix.areLinearlyIndependent listOfRowVectors)
        , Test.test "tests matrix doesSetSpanSpace with standard basis vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 1 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfRowVectors
                in
                Expect.true "Vector spans R2" result
        , Test.test "tests matrix doesSetSpanSpace with zero vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfRowVectors
                in
                Expect.false "Vector does not span R2" result
        , Test.test "tests matrix doesSetSpanSpace with zero vectors 3 dimensions" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 0, 1 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.doesSetSpanSpace r3 listOfRowVectors
                in
                Expect.true "Vector spans R3" result
        , Test.test "tests matrix doesSetSpanSpace with three vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.doesSetSpanSpace r3 listOfRowVectors
                in
                Expect.false "Vector does not spans R3" result
        , Test.test "tests matrix doesSetSpanSpace with three vectors testing r2" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfRowVectors
                in
                Expect.false "Vector does not span R2" result
        , Test.test "tests matrix doesSetSpanSpace with three dimensional vector against R2" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 2, 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.doesSetSpanSpace r2 listOfRowVectors
                in
                Expect.false "Vector does not spans R2" result
        , Test.test "tests matrix areBasis with standard basis vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 1 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfRowVectors
                in
                Expect.true "Vectors are basis for R2" result
        , Test.test "tests matrix areBasis with zero vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfRowVectors
                in
                Expect.false "Vectors are not basis for R2" result
        , Test.test "tests matrix areBasis with standard basis vectors 3 dimensions" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.areBasis r3 listOfRowVectors
                in
                Expect.false "Vectors are not basis for R3" result
        , Test.test "tests matrix areBasis with zero vectors 3 dimensions" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 0, 0, 1 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.areBasis r3 listOfRowVectors
                in
                Expect.true "Vectors are basis fro R3" result
        , Test.test "tests matrix areBasis with three vectors" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        ]

                    r3 =
                        Matrix.VectorSpace 3

                    result =
                        Matrix.areBasis r3 listOfRowVectors
                in
                Expect.false "Vectors are not basis R3" result
        , Test.test "tests matrix areBasis with three vectors testing r2" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 1, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfRowVectors
                in
                Expect.false "Vectors are not basis for R2" result
        , Test.test "tests matrix areBasis with three dimensional vector against R2" <|
            \_ ->
                let
                    listOfRowVectors =
                        [ Matrix.RowVector <| Vector.Vector [ 1, 0, 0 ]
                        , Matrix.RowVector <| Vector.Vector [ 2, 0, 0 ]
                        ]

                    r2 =
                        Matrix.VectorSpace 2

                    result =
                        Matrix.areBasis r2 listOfRowVectors
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

                    reducedRowEchelonFormMatrix =
                        Matrix.solveMatrix matrix
                in
                Expect.equal reducedRowEchelonFormMatrix (Matrix.InfiniteSolutions { nullity = 3, rank = 2 })
        ]
