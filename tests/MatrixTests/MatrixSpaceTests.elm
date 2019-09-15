module MatrixTests.MatrixSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Space Tests"
        [ Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scalar multiplication distributes over addition" <|
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
                        Matrix.addMatrices ComplexNumbers.complexField v w

                    cvPlusW =
                        Matrix.map (ComplexNumbers.multiply c) vPlusW

                    cW =
                        Matrix.map (ComplexNumbers.multiply c) w

                    cV =
                        Matrix.map (ComplexNumbers.multiply c) v

                    cVPluscW =
                        Matrix.addMatrices ComplexNumbers.complexField cW cV

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
                        Matrix.addMatrices ComplexNumbers.complexField m1 m2
                            |> Matrix.transpose

                    m1TransposePlusm2Transpose =
                        Matrix.transpose m1
                            |> Matrix.addMatrices ComplexNumbers.complexField (Matrix.transpose m2)
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
                        Matrix.addMatrices ComplexNumbers.complexField m1 m2
                            |> Matrix.conjugate

                    m1ConjugatePlusm2Conjugate =
                        Matrix.conjugate m1
                            |> Matrix.addMatrices ComplexNumbers.complexField (Matrix.conjugate m2)
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
                        Matrix.addMatrices ComplexNumbers.complexField m1 m2
                            |> Matrix.adjoint

                    m1ConjugatePlusm2Adjoint =
                        Matrix.adjoint m1
                            |> Matrix.addMatrices ComplexNumbers.complexField (Matrix.conjugate m2)
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
                        Matrix.multiplyMatrices Vector.realInnerProductSpace m1 m2
                            |> Result.map (Matrix.map ((*) one))

                    cTimesm1ThenTimesm2 =
                        Matrix.multiplyMatrices Vector.realInnerProductSpace (Matrix.map ((*) one) m1) m2
                in
                Expect.equal cTimesm1Timem2 cTimesm1ThenTimesm2
        ]
