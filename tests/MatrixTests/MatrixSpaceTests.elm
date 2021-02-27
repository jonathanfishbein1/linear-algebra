module MatrixTests.MatrixSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
import Matrix
import Real
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Space Tests"
        [ Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests one is product identity"
          <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Expect.true "equal" ((Internal.Vector.equal ComplexNumbers.equal.eq).eq (Internal.Vector.map (ComplexNumbers.multiply ComplexNumbers.one) v) v)
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests scalar multiplication respects complex multiplication"
          <|
            \one two ->
                let
                    c1 =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    c2 =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary
                                one
                            )

                    v =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    c2V =
                        Matrix.scalarMultiplication ComplexNumbers.field c2 v

                    c2VThenc1 =
                        Matrix.scalarMultiplication ComplexNumbers.field c1 c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        Matrix.scalarMultiplication ComplexNumbers.field c1c2 v
                in
                c2VThenc1
                    |> Expect.equal c1c2ThenV
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix scalar multiplication distributes over addition"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    w =
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

                    v =
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

                    vPlusW =
                        Matrix.add ComplexNumbers.field v w

                    cvPlusW =
                        Matrix.scalarMultiplication ComplexNumbers.field c vPlusW

                    cW =
                        Matrix.scalarMultiplication ComplexNumbers.field c w

                    cV =
                        Matrix.scalarMultiplication ComplexNumbers.field c v

                    cVPluscW =
                        Matrix.add ComplexNumbers.field cW cV

                    result =
                        (Matrix.equal ComplexNumbers.equal.eq).eq cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests scalar multiplication distributes over addition"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    w =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        two
                                        (Imaginary.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    v =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumber
                                        one
                                        (Imaginary.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    vPlusW =
                        Matrix.add ComplexNumbers.field v w

                    cvPlusW =
                        Matrix.scalarMultiplication ComplexNumbers.field c vPlusW

                    cW =
                        Matrix.scalarMultiplication ComplexNumbers.field c w

                    cV =
                        Matrix.scalarMultiplication ComplexNumbers.field c v

                    cVPluscW =
                        Matrix.add ComplexNumbers.field cW cV

                    result =
                        (Matrix.equal ComplexNumbers.equal.eq).eq cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        ]
