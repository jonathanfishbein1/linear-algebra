module MatrixTests.ComplexAlgebraTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Complex Algebra"
        [ Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests real Matrix multiplication is associative"
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
                        Result.andThen (Matrix.multiplyMatrices Vector.realInnerProductSpace m3)
                            (Matrix.multiplyMatrices Vector.realInnerProductSpace m1 m2)

                    m2Timesm3AndThenTimesm1 =
                        Result.andThen (Matrix.multiplyMatrices Vector.realInnerProductSpace m1)
                            (Matrix.multiplyMatrices Vector.realInnerProductSpace m2 m3)
                in
                Expect.equal m1Timesm2AndThenTimesm3 m2Timesm3AndThenTimesm1
        , Test.test
            "tests identityMatrix is an identity matrix"
          <|
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
        , Test.fuzz3
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            "tests In*A = A"
          <|
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
                        Matrix.multiplyMatrices Vector.realInnerProductSpace (Matrix.identityMatrix Field.realField 3) m1
                in
                Expect.equal m1TimeI (Ok m1)
        , Test.fuzz3
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            "tests A*In = a"
          <|
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
                        Matrix.multiplyMatrices Vector.realInnerProductSpace m1 (Matrix.identityMatrix Field.realField 3)
                in
                Expect.equal m1TimeI (Ok m1)
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests real Matrix multiplication distributes over addition"
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
                        Matrix.multiplyMatrices Vector.realInnerProductSpace m1 (Matrix.addMatrices Field.realField m2 m3)

                    m1Timesm2Plusem1Timesm3 =
                        Result.map2 (Matrix.addMatrices Field.realField) (Matrix.multiplyMatrices Vector.realInnerProductSpace m1 m2) (Matrix.multiplyMatrices Vector.realInnerProductSpace m1 m3)
                in
                Expect.equal m1Timesm2Plus3 m1Timesm2Plusem1Timesm3
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests real Matrix multiplication distributes over addition second test"
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
                        Matrix.multiplyMatrices Vector.realInnerProductSpace (Matrix.addMatrices Field.realField m2 m3) m1

                    m2Timesm1Plusm3Timesm1 =
                        Result.map2 (Matrix.addMatrices Field.realField) (Matrix.multiplyMatrices Vector.realInnerProductSpace m2 m1) (Matrix.multiplyMatrices Vector.realInnerProductSpace m3 m1)
                in
                Expect.equal m2Plusm3Timesm1 m2Timesm1Plusm3Timesm1
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests matrix multiplication relates to the transpose"
          <|
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
                        Matrix.multiplyMatrices Vector.realInnerProductSpace a b
                            |> Result.map Matrix.transpose

                    cTimesm1ThenTimesm2 =
                        Matrix.multiplyMatrices Vector.realInnerProductSpace (Matrix.transpose b) (Matrix.transpose a)
                in
                Expect.equal aTimebThenTranspose cTimesm1ThenTimesm2
        , Test.test
            "tests matrix vector multiplication"
          <|
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
                        Matrix.multiplyMatrixVector Vector.realInnerProductSpace m v

                    expected =
                        Vector.Vector [ 14, 32, 50 ]
                in
                Expect.equal mTimesV (Ok expected)
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests matrix multiplication respects the conjugate"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real three)
                            (ComplexNumbers.Imaginary two)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real two)
                            (ComplexNumbers.Imaginary three)

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
                        Matrix.multiplyMatrices Vector.complexInnerProductSpace a b
                            |> Result.map Matrix.conjugate

                    cTimesm1ThenTimesm2 =
                        Matrix.multiplyMatrices Vector.complexInnerProductSpace (Matrix.conjugate a) (Matrix.conjugate b)

                    result =
                        Result.map2 (Matrix.equal ComplexNumbers.equal) aTimebThenConjugate cTimesm1ThenTimesm2
                in
                Expect.equal result (Ok True)
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests matrix multiplication relates to the adjoint"
          <|
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
                        Matrix.multiplyMatrices Vector.complexInnerProductSpace a b
                            |> Result.map Matrix.adjoint

                    bAdjointTimesAAdjoint =
                        Matrix.multiplyMatrices Vector.complexInnerProductSpace (Matrix.adjoint a) (Matrix.adjoint b)

                    result =
                        Result.map2 (Matrix.equal ComplexNumbers.equal) aTimebThenAdjoint bAdjointTimesAAdjoint
                in
                Expect.equal result (Ok True)
        ]
