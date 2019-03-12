module MatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Float.Extra
import Fuzz
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
                            [ Vector.Vector
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
                        Matrix.Matrix [ Vector.Vector [ ComplexNumbers.zero ] ]
                in
                Monoid.append (Matrix.sumComplexMatrices sumEmptyComplexMatrix) m (Monoid.empty <| Matrix.sumComplexMatrices sumEmptyComplexMatrix)
                    |> Expect.equal m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add matricies" <|
            \one two three ->
                let
                    a =
                        Matrix.Matrix [ Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two) ] ]

                    b =
                        Matrix.Matrix [ Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three) ] ]

                    c =
                        Matrix.Matrix [ Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three) ] ]

                    expected =
                        Matrix.addComplexMatrices (Matrix.addComplexMatrices a b) c

                    sumEmptyComplexMatrix =
                        Matrix.Matrix [ Vector.Vector [ ComplexNumbers.zero ] ]

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
                            [ Vector.Vector
                                [ ComplexNumbers.one
                                ]
                            ]

                    w =
                        Matrix.Matrix
                            [ Vector.Vector
                                [ ComplexNumbers.negate ComplexNumbers.one
                                ]
                            ]

                    zero =
                        Matrix.Matrix
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                            [ Vector.Vector
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
                        Vector.Vector
                            [ three
                            , one
                            , three
                            , two
                            ]

                    v2 =
                        Vector.Vector
                            [ one
                            , three
                            , three
                            , two
                            ]

                    v3 =
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
                        Vector.Vector
                            [ 1
                            , 0
                            , 0
                            ]

                    v2 =
                        Vector.Vector
                            [ 0
                            , 1
                            , 0
                            ]

                    v3 =
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
                        Vector.Vector
                            [ three
                            , one
                            , three
                            ]

                    v2 =
                        Vector.Vector
                            [ one
                            , three
                            , three
                            ]

                    v3 =
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
                        Vector.Vector
                            [ three
                            , one
                            , three
                            ]

                    v2 =
                        Vector.Vector
                            [ one
                            , three
                            , three
                            ]

                    v3 =
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
        ]
