module MatrixTests.ComplexAlgebraTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
import Test
import Vector
import Field
 
suite : Test.Test
suite =
    Test.describe "Complex Algebra"
        [ Test.describe "Complex Matrix Space"
            [ Test.describe "Abelian Group"
                [ Test.fuzz3 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests Matrix add is commutative" <|
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
                        (Matrix.addMatrices Field.complexField) m1 m2
                            |> Expect.equal ((Matrix.addMatrices Field.complexField) m2 m1)
                , Test.fuzz3 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests Matrix add is associative" <|
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
                                (Matrix.addMatrices Field.complexField) m1 m2
                                    |> (Matrix.addMatrices Field.complexField) m3

                            m2Plusm3AndThenm1 =
                                (Matrix.addMatrices Field.complexField) m2 m3
                                    |> (Matrix.addMatrices Field.complexField) m1
                        in
                        m1Plusm2AndThenPlusm3
                            |> Expect.equal m2Plusm3AndThenm1
                , Test.fuzz2 Fuzz.int Fuzz.int "tests matrix inverse" <|
                    \one two ->
                        let
                            complexOneNegative =
                                ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real <|
                                        Basics.negate
                                            1
                                    )
                                    (ComplexNumbers.Imaginary
                                        0
                                    )

                            v =
                                Matrix.Matrix
                                    [ Matrix.RowVector <|
                                        Vector.Vector
                                            [ ComplexNumbers.one ]
                                    ]

                            w =
                                Matrix.Matrix
                                    [ Matrix.RowVector <|
                                        Vector.Vector
                                            [ complexOneNegative ]
                                    ]

                            zero =
                                Matrix.Matrix
                                    [ Matrix.RowVector <|
                                        Vector.Vector
                                            [ ComplexNumbers.zero
                                            ]
                                    ]
                        in
                        (Matrix.addMatrices Field.complexField) v w
                            |> Expect.equal zero
                ]
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
                            (Matrix.addMatrices Field.complexField) v w

                        cvPlusW =
                            Matrix.map (ComplexNumbers.multiply c) vPlusW

                        cW =
                            Matrix.map (ComplexNumbers.multiply c) w

                        cV =
                            Matrix.map (ComplexNumbers.multiply c) v

                        cVPluscW =
                            (Matrix.addMatrices Field.complexField) cW cV

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
                            (Matrix.addMatrices Field.complexField) m1 m2
                                |> Matrix.transpose

                        m1TransposePlusm2Transpose =
                            Matrix.transpose m1
                                |> (Matrix.addMatrices Field.complexField) (Matrix.transpose m2)
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
                            (Matrix.addMatrices Field.complexField) m1 m2
                                |> Matrix.conjugate

                        m1ConjugatePlusm2Conjugate =
                            Matrix.conjugate m1
                                |> (Matrix.addMatrices Field.complexField) (Matrix.conjugate m2)
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
                            (Matrix.addMatrices Field.complexField) m1 m2
                                |> Matrix.adjoint

                        m1ConjugatePlusm2Adjoint =
                            Matrix.adjoint m1
                                |> (Matrix.addMatrices Field.complexField) (Matrix.conjugate m2)
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
                            (Matrix.multiplyMatrices Vector.realInnerProductSpace) m1 m2
                                |> Result.map (Matrix.map ((*) one))

                        cTimesm1ThenTimesm2 =
                            (Matrix.multiplyMatrices Vector.realInnerProductSpace) (Matrix.map ((*) one) m1) m2
                    in
                    Expect.equal cTimesm1Timem2 cTimesm1ThenTimesm2
            ]
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
                        Result.andThen ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m3)
                            ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m1 m2)

                    m2Timesm3AndThenTimesm1 =
                        Result.andThen ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m1)
                            ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m2 m3)
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
                Expect.equal (Matrix.identityMatrix Field.realField 3) m1
        , Test.fuzz3 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests In*A = A" <|
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
                        (Matrix.multiplyMatrices Vector.realInnerProductSpace) (Matrix.identityMatrix Field.realField 3) m1
                in
                Expect.equal m1TimeI (Ok m1)
        , Test.fuzz3 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests A*In = a" <|
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
                        (Matrix.multiplyMatrices Vector.realInnerProductSpace) m1 (Matrix.identityMatrix Field.realField 3)
                in
                Expect.equal m1TimeI (Ok m1)
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
                        (Matrix.multiplyMatrices Vector.realInnerProductSpace) m1 (Matrix.addMatrices Field.realField m2 m3)

                    m1Timesm2Plusem1Timesm3 =
                        Result.map2 (Matrix.addMatrices Field.realField) ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m1 m2) ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m1 m3)
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
                        (Matrix.multiplyMatrices Vector.realInnerProductSpace) ((Matrix.addMatrices Field.realField) m2 m3) m1

                    m2Timesm1Plusm3Timesm1 =
                        Result.map2 (Matrix.addMatrices Field.realField) ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m2 m1) ((Matrix.multiplyMatrices Vector.realInnerProductSpace) m3 m1)
                in
                Expect.equal m2Plusm3Timesm1 m2Timesm1Plusm3Timesm1
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
                        (Matrix.multiplyMatrices Vector.realInnerProductSpace) a b
                            |> Result.map Matrix.transpose

                    cTimesm1ThenTimesm2 =
                        (Matrix.multiplyMatrices Vector.realInnerProductSpace) (Matrix.transpose b) (Matrix.transpose a)
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
                        Vector.Vector [ 14, 32, 50 ]
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
                        Matrix.Matrix
                            [ v1
                            , v2
                            ]

                    b =
                        Matrix.Matrix
                            [ v2
                            , v1
                            ]

                    aTimebThenConjugate =
                        (Matrix.multiplyMatrices Vector.complexInnerProductSpace) a b 
                            |> Result.map Matrix.conjugate

                    cTimesm1ThenTimesm2 =
                        (Matrix.multiplyMatrices Vector.complexInnerProductSpace) (Matrix.conjugate a) (Matrix.conjugate b)

                    result =
                        Result.map2 (Matrix.equal ComplexNumbers.equal) aTimebThenConjugate cTimesm1ThenTimesm2
                in
                Expect.equal result (Ok True)
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix multiplication relates to the adjoint" <|
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
                        (Matrix.multiplyMatrices Vector.complexInnerProductSpace) a b
                            |> Result.map Matrix.adjoint

                    bAdjointTimesAAdjoint =
                        (Matrix.multiplyMatrices Vector.complexInnerProductSpace) (Matrix.adjoint a) (Matrix.adjoint b)

                    result =
                        Result.map2 (Matrix.equal ComplexNumbers.equal) aTimebThenAdjoint bAdjointTimesAAdjoint
                in
                Expect.equal result (Ok True)
        ]
