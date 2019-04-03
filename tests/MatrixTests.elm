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

        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication is associative" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three
        --                         , one
        --                         , three
        --                         , two
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one
        --                         , three
        --                         , three
        --                         , two
        --                         ]
        --             v3 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ two
        --                         , three
        --                         , one
        --                         , two
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1 ]
        --             m2 =
        --                 Matrix.Matrix [ v2 ]
        --             m3 =
        --                 Matrix.Matrix [ v3 ]
        --             m1Timesm2AndThenTimesm3 =
        --                 Matrix.multiplyRealMatrices (Matrix.multiplyRealMatrices m1 m2) m3
        --             m2Timesm3AndThenTimesm1 : Matrix.Matrix number
        --             m2Timesm3AndThenTimesm1 =
        --                 Matrix.multiplyRealMatrices m1 (Matrix.multiplyRealMatrices m2 m3)
        --         in
        --         Expect.equal m1Timesm2AndThenTimesm3 m2Timesm3AndThenTimesm1
        -- , Test.test "tests identityMatrix is an identity matrix" <|
        --     \_ ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ 1
        --                         , 0
        --                         , 0
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ 0
        --                         , 1
        --                         , 0
        --                         ]
        --             v3 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ 0
        --                         , 0
        --                         , 1
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1, v2, v3 ]
        --         in
        --         Expect.equal (Matrix.identityMatrix 3) m1
        -- , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests In*A = A" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three
        --                         , one
        --                         , three
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one
        --                         , three
        --                         , three
        --                         ]
        --             v3 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ two
        --                         , three
        --                         , one
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1, v2, v3 ]
        --             m1TimeI =
        --                 Matrix.multiplyRealMatrices m1 (Matrix.identityMatrix 3)
        --         in
        --         Expect.equal m1TimeI m1
        -- , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests A*In = a" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three
        --                         , one
        --                         , three
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one
        --                         , three
        --                         , three
        --                         ]
        --             v3 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ two
        --                         , three
        --                         , one
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1, v2, v3 ]
        --             m1TimeI =
        --                 Matrix.multiplyRealMatrices (Matrix.identityMatrix 3) m1
        --         in
        --         Expect.equal m1TimeI m1
        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication distributes over addition" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three
        --                         , one
        --                         , three
        --                         , two
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one
        --                         , three
        --                         , three
        --                         , two
        --                         ]
        --             v3 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ two
        --                         , three
        --                         , one
        --                         , two
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1 ]
        --             m2 =
        --                 Matrix.Matrix [ v2 ]
        --             m3 =
        --                 Matrix.Matrix [ v3 ]
        --             m1Timesm2Plus3 =
        --                 Matrix.multiplyRealMatrices m1 (Matrix.addRealMatrices m2 m3)
        --             m1Timesm2Plusem1Timesm3 =
        --                 Matrix.addRealMatrices (Matrix.multiplyRealMatrices m1 m2) (Matrix.multiplyRealMatrices m1 m3)
        --         in
        --         Expect.equal m1Timesm2Plus3 m1Timesm2Plusem1Timesm3
        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication distributes over addition second test" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three
        --                         , one
        --                         , three
        --                         , two
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one
        --                         , three
        --                         , three
        --                         , two
        --                         ]
        --             v3 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ two
        --                         , three
        --                         , one
        --                         , two
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1 ]
        --             m2 =
        --                 Matrix.Matrix [ v2 ]
        --             m3 =
        --                 Matrix.Matrix [ v3 ]
        --             m2Plusm3Timesm1 =
        --                 Matrix.multiplyRealMatrices (Matrix.addRealMatrices m2 m3) m1
        --             m2Timesm1Plusm3Timesm1 =
        --                 Matrix.addRealMatrices (Matrix.multiplyRealMatrices m2 m1) (Matrix.multiplyRealMatrices m3 m1)
        --         in
        --         Expect.equal m2Plusm3Timesm1 m2Timesm1Plusm3Timesm1
        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests real Matrix multiplication respects scalar multiplication" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three
        --                         , one
        --                         , three
        --                         , two
        --                         ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one
        --                         , three
        --                         , three
        --                         , two
        --                         ]
        --             m1 =
        --                 Matrix.Matrix
        --                     [ v1 ]
        --             m2 =
        --                 Matrix.Matrix [ v2 ]
        --             cTimesm1Timem2 =
        --                 Matrix.multiplyRealMatrices m1 m2
        --                     |> Matrix.map ((*) one)
        --             cTimesm1ThenTimesm2 =
        --                 Matrix.multiplyRealMatrices (Matrix.map ((*) one) m1) m2
        --         in
        --         Expect.equal cTimesm1Timem2 cTimesm1ThenTimesm2
        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication relates to the transpose" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three, one ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one, two ]
        --             a =
        --                 Matrix.Matrix [ v1 ]
        --             b =
        --                 Matrix.Matrix [ v2 ]
        --             aTimebThenTranspose =
        --                 Matrix.multiplyRealMatrices a b
        --                     |> Matrix.transpose
        --             cTimesm1ThenTimesm2 =
        --                 Matrix.multiplyRealMatrices (Matrix.transpose b) (Matrix.transpose a)
        --         in
        --         Expect.equal aTimebThenTranspose cTimesm1ThenTimesm2
        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication respects the conjugate" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three, one ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one, two ]
        --             a =
        --                 Matrix.Matrix [ v1 ]
        --             b =
        --                 Matrix.Matrix [ v2 ]
        --             aTimebThenConjugate : Matrix.Matrix (ComplexNumbers.ComplexNumberCartesian number)
        --             aTimebThenConjugate =
        --                 Matrix.multiplyComplexMatrices a b
        --                     |> Matrix.conjugate
        --             cTimesm1ThenTimesm2 : Matrix.Matrix (ComplexNumbers.ComplexNumberCartesian number)
        --             cTimesm1ThenTimesm2 =
        --                 Matrix.multiplyComplexMatrices (Matrix.conjugate a) (Matrix.conjugate b)
        --             result =
        --                 Matrix.equal ComplexNumbers.equal aTimebThenConjugate cTimesm1ThenTimesm2
        --         in
        --         Expect.true "AB conjugate equals A conjugate time B conjugate" result
        -- , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication relates to the adjoin" <|
        --     \one two three ->
        --         let
        --             v1 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ three, one ]
        --             v2 =
        --                 Matrix.RowVector <|
        --                     Vector.Vector
        --                         [ one, two ]
        --             a =
        --                 Matrix.Matrix [ v1 ]
        --             b =
        --                 Matrix.Matrix [ v2 ]
        --             aTimebThenAdjoint : Matrix.Matrix (ComplexNumbers.ComplexNumberCartesian number)
        --             aTimebThenAdjoint =
        --                 Matrix.multiplyComplexMatrices a b
        --                     |> Matrix.adjoint
        --             bAdjointTimesAAdjoint : Matrix.Matrix (ComplexNumbers.ComplexNumberCartesian number)
        --             bAdjointTimesAAdjoint =
        --                 Matrix.multiplyComplexMatrices (Matrix.adjoint a) (Matrix.adjoint b)
        --             result =
        --                 Matrix.equal ComplexNumbers.equal aTimebThenAdjoint bAdjointTimesAAdjoint
        --         in
        --         Expect.true "AB adjoint equals A conjugate time B adjoint" result
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix swap applied twice is the same matrix" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, two ]
                            , Matrix.RowVector <| Vector.Vector [ two, one ]
                            , Matrix.RowVector <| Vector.Vector [ two, one ]
                            ]

                    swapOneTwo =
                        Matrix.swap m1 1 2

                    swapAgain =
                        Matrix.swap swapOneTwo 1 2
                in
                Expect.equal m1 swapAgain
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix swap does not change matrix length" <|
            \one two ->
                let
                    m1 =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, two ]
                            , Matrix.RowVector <| Vector.Vector [ two, one ]
                            , Matrix.RowVector <| Vector.Vector [ two, one ]
                            ]

                    (Matrix.Matrix m1List) =
                        m1

                    m1Length =
                        List.length m1List

                    swapOneTwo =
                        Matrix.swap m1 1 2

                    (Matrix.Matrix swapOneTwoList) =
                        swapOneTwo

                    swapOneLength =
                        List.length swapOneTwoList
                in
                Expect.equal m1Length swapOneLength
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
                            [ Matrix.RowVector <| Vector.Vector [ 1, 2, -1, -4 ]
                            , Matrix.RowVector <| Vector.Vector [ 2, 3, -1, -11 ]
                            , Matrix.RowVector <| Vector.Vector [ -2, 0, -3, 22 ]
                            ]

                    reducedRowEchelonFormMatrix =
                        Matrix.solve matrix

                    expected =
                        Matrix.ColumnVector <| Vector.Vector [ -8.0, 1.0, -2.0 ]
                in
                Expect.equal reducedRowEchelonFormMatrix expected
        ]
