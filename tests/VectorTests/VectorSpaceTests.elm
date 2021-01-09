module VectorTests.VectorSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Vector Space Tests"
        [ Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests one is product identity"
          <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                Expect.true "equal" (Vector.equal ComplexNumbers.equal (Vector.map (ComplexNumbers.multiply ComplexNumbers.one) v) v)
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests scalar multiplication respects complex multiplication"
          <|
            \one two ->
                let
                    c1 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    c2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                two
                            )
                            (ComplexNumbers.Imaginary
                                one
                            )

                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    c2V =
                        Vector.scalarMultiplication ComplexNumbers.complexField c2 v

                    c2VThenc1 =
                        Vector.scalarMultiplication ComplexNumbers.complexField c1 c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        Vector.scalarMultiplication ComplexNumbers.complexField c1c2 v
                in
                c2VThenc1
                    |> Expect.equal c1c2ThenV
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests scalar multiplication distributes over addition"
          <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]

                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    vPlusW =
                        Vector.add ComplexNumbers.complexField v w

                    cvPlusW =
                        Vector.scalarMultiplication ComplexNumbers.complexField c vPlusW

                    cW =
                        Vector.scalarMultiplication ComplexNumbers.complexField c w

                    cV =
                        Vector.scalarMultiplication ComplexNumbers.complexField c v

                    cVPluscW =
                        Vector.add ComplexNumbers.complexField cW cV

                    result =
                        Vector.equal ComplexNumbers.equal cvPlusW cVPluscW
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
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    c2 =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                two
                            )
                            (ComplexNumbers.Imaginary
                                one
                            )

                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    c1Plusc2 =
                        ComplexNumbers.add c1 c2

                    c1Plusc2V =
                        Vector.scalarMultiplication ComplexNumbers.complexField c1Plusc2 v

                    c1V =
                        Vector.scalarMultiplication ComplexNumbers.complexField c1 v

                    c2V =
                        Vector.scalarMultiplication ComplexNumbers.complexField c2 v

                    c1VPlusc2V =
                        Vector.add ComplexNumbers.complexField c1V c2V

                    result =
                        Vector.equal ComplexNumbers.equal c1VPlusc2V c1Plusc2V
                in
                Expect.true "All elements equal" result
        ]
