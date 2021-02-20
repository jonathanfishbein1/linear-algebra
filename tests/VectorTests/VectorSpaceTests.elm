module VectorTests.VectorSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Vector Space Tests"
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
                Expect.true "equal" ((Vector.equal ComplexNumbers.equal.eq).eq (Vector.map (ComplexNumbers.multiply ComplexNumbers.one) v) v)
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
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]

                    c2V =
                        Vector.scalarMultiplication ComplexNumbers.field c2 v

                    c2VThenc1 =
                        Vector.scalarMultiplication ComplexNumbers.field c1 c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        Vector.scalarMultiplication ComplexNumbers.field c1c2 v
                in
                c2VThenc1
                    |> Expect.equal c1c2ThenV
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
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                two
                                (Imaginary.Imaginary
                                    one
                                )
                            ]

                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]

                    vPlusW =
                        Vector.add ComplexNumbers.field v w

                    cvPlusW =
                        Vector.scalarMultiplication ComplexNumbers.field c vPlusW

                    cW =
                        Vector.scalarMultiplication ComplexNumbers.field c w

                    cV =
                        Vector.scalarMultiplication ComplexNumbers.field c v

                    cVPluscW =
                        Vector.add ComplexNumbers.field cW cV

                    result =
                        (Vector.equal ComplexNumbers.equal.eq).eq cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests scalar multiplication distributes over complex addition"
          <|
            \one two ->
                let
                    c1 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                one
                            )
                            (Imaginary.Imaginary
                                two
                            )

                    c2 =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                two
                            )
                            (Imaginary.Imaginary
                                one
                            )

                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (Real.Real
                                    one
                                )
                                (Imaginary.Imaginary
                                    two
                                )
                            ]

                    c1Plusc2 =
                        ComplexNumbers.add c1 c2

                    c1Plusc2V =
                        Vector.scalarMultiplication ComplexNumbers.field c1Plusc2 v

                    c1V =
                        Vector.scalarMultiplication ComplexNumbers.field c1 v

                    c2V =
                        Vector.scalarMultiplication ComplexNumbers.field c2 v

                    c1VPlusc2V =
                        Vector.add ComplexNumbers.field c1V c2V

                    result =
                        (Vector.equal ComplexNumbers.equal.eq).eq c1VPlusc2V c1Plusc2V
                in
                Expect.true "All elements equal" result
        ]
