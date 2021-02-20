module MatrixTests.ComplexAlgebraTests exposing (suite)

import ColumnVector
import ComplexNumbers
import Expect
import Field
import Fuzz
import Imaginary
import Matrix
import Real
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Complex Algebra"
        [ Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests real Matrix multiplication is associative"
          <|
            \one two three ->
                let
                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                , two
                                ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                , two
                                ]

                    v3 =
                        RowVector.RowVector <|
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
                        Result.andThen (Matrix.multiply Vector.realInnerProductSpace m3)
                            (Matrix.multiply Vector.realInnerProductSpace m1 m2)

                    m2Timesm3AndThenTimesm1 =
                        Result.andThen (Matrix.multiply Vector.realInnerProductSpace m1)
                            (Matrix.multiply Vector.realInnerProductSpace m2 m3)
                in
                Expect.equal m1Timesm2AndThenTimesm3 m2Timesm3AndThenTimesm1
        , Test.test
            "tests identity is an identity matrix"
          <|
            \_ ->
                let
                    v1 =
                        Vector.Vector
                            [ 1
                            , 0
                            , 0
                            ]
                            |> RowVector.RowVector
                            |> RowVector.map Real.Real

                    v2 =
                        Vector.Vector
                            [ 0
                            , 1
                            , 0
                            ]
                            |> RowVector.RowVector
                            |> RowVector.map Real.Real

                    v3 =
                        Vector.Vector
                            [ 0
                            , 0
                            , 1
                            ]
                            |> RowVector.RowVector
                            |> RowVector.map Real.Real

                    m1 =
                        Matrix.Matrix
                            [ v1, v2, v3 ]
                in
                Expect.equal (Matrix.identity Real.field 3) m1
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests In*A = A"
          <|
            \one two three ->
                let
                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                ]

                    v3 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1, v2, v3 ]

                    m1TimeI =
                        Matrix.multiply Vector.realInnerProductSpace (Matrix.identity Real.field 3) m1
                in
                Expect.equal m1TimeI (Ok m1)
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests A*In = a"
          <|
            \one two three ->
                let
                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                ]

                    v3 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ two
                                , three
                                , one
                                ]

                    m1 =
                        Matrix.Matrix
                            [ v1, v2, v3 ]

                    m1TimeI =
                        Matrix.multiply Vector.realInnerProductSpace m1 (Matrix.identity Real.field 3)
                in
                Expect.equal m1TimeI (Ok m1)
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests real Matrix multiplication distributes over addition"
          <|
            \one two three ->
                let
                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                , two
                                ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                , two
                                ]

                    v3 =
                        RowVector.RowVector <|
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
                        Matrix.multiply Vector.realInnerProductSpace m1 (Matrix.add Real.field m2 m3)

                    m1Timesm2Plusem1Timesm3 =
                        Result.map2 (Matrix.add Real.field) (Matrix.multiply Vector.realInnerProductSpace m1 m2) (Matrix.multiply Vector.realInnerProductSpace m1 m3)
                in
                Expect.equal m1Timesm2Plus3 m1Timesm2Plusem1Timesm3
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests real Matrix multiplication distributes over addition second test"
          <|
            \one two three ->
                let
                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ three
                                , one
                                , three
                                , two
                                ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ one
                                , three
                                , three
                                , two
                                ]

                    v3 =
                        RowVector.RowVector <|
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
                        Matrix.multiply Vector.realInnerProductSpace (Matrix.add Real.field m2 m3) m1

                    m2Timesm1Plusm3Timesm1 =
                        Result.map2
                            (Matrix.add Real.field)
                            (Matrix.multiply Vector.realInnerProductSpace m2 m1)
                            (Matrix.multiply Vector.realInnerProductSpace m3 m1)
                in
                Expect.equal m2Plusm3Timesm1 m2Timesm1Plusm3Timesm1
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests matrix multiplication relates to the transpose"
          <|
            \one two three ->
                let
                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ three, one ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ one, two ]

                    a =
                        Matrix.Matrix [ v1 ]

                    b =
                        Matrix.Matrix [ v2 ]

                    aTimebThenTranspose =
                        Matrix.multiply Vector.realInnerProductSpace a b
                            |> Result.map Matrix.transpose

                    cTimesm1ThenTimesm2 =
                        Matrix.multiply Vector.realInnerProductSpace (Matrix.transpose b) (Matrix.transpose a)
                in
                Expect.equal aTimebThenTranspose cTimesm1ThenTimesm2
        , Test.test
            "tests matrix vector multiplication"
          <|
            \_ ->
                let
                    v =
                        Vector.Vector
                            [ 1
                            , 2
                            , 3
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real

                    m =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 1, 2, 3 ]
                            , RowVector.RowVector <| Vector.Vector [ 4, 5, 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 7, 8, 9 ]
                            ]
                            |> Matrix.map Real.Real

                    mTimesV =
                        Matrix.multiplyMatrixVector Vector.realInnerProductSpace m v

                    expected =
                        Vector.Vector
                            [ 14
                            , 32
                            , 50
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                in
                Expect.equal mTimesV (Ok expected)
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests matrix multiplication respects the conjugate"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            three
                            (Imaginary.Imaginary two)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ complexNumberThree, complexNumberOne ]

                    v2 =
                        RowVector.RowVector <|
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
                        Matrix.multiply Vector.complexInnerProductSpace a b
                            |> Result.map Matrix.conjugate

                    cTimesm1ThenTimesm2 =
                        Matrix.multiply Vector.complexInnerProductSpace (Matrix.conjugate a) (Matrix.conjugate b)

                    result =
                        Result.map2
                            (Matrix.equal ComplexNumbers.equal.eq).eq
                            aTimebThenConjugate
                            cTimesm1ThenTimesm2
                in
                Expect.equal result (Ok True)
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests matrix multiplication relates to the adjoint"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber one (Imaginary.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber three (Imaginary.Imaginary two)

                    v1 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ complexNumberOne ]

                    v2 =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ complexNumberTwo ]

                    a =
                        Matrix.Matrix [ v1 ]

                    b =
                        Matrix.Matrix [ v2 ]

                    aTimebThenAdjoint =
                        Matrix.multiply Vector.complexInnerProductSpace a b
                            |> Result.map Matrix.adjoint

                    bAdjointTimesAAdjoint =
                        Matrix.multiply Vector.complexInnerProductSpace (Matrix.adjoint a) (Matrix.adjoint b)

                    result =
                        Result.map2
                            (Matrix.equal ComplexNumbers.equal.eq).eq
                            aTimebThenAdjoint
                            bAdjointTimesAAdjoint
                in
                Expect.equal result (Ok True)
        ]
