module VectorTests.VectorMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests empty vector is additive identity"
          <|
            \one two ->
                let
                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Internal.Vector.add ComplexNumbers.field (Vector.Vector [ ComplexNumbers.zero ]) w
                    |> Expect.equal w
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests Vector empty or identity value right"
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
                if (Internal.Vector.equal ComplexNumbers.equal.eq).eq (Internal.Vector.concat.semigroup v Internal.Vector.concat.identity) v then
                    Expect.pass

                else
                    Expect.fail "vectors not equal"
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests Vector empty or identity value left"
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
                if (Internal.Vector.equal ComplexNumbers.equal.eq).eq (Internal.Vector.concat.semigroup Internal.Vector.concat.identity v) v then
                    Expect.pass

                else
                    Expect.fail "vectors not equal"
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            "tests monoidally concat complex vectors"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary three)

                    a =
                        Vector.Vector [ complexNumberOne ]

                    b =
                        Vector.Vector [ complexNumberTwo ]

                    c =
                        Vector.Vector [ complexNumberThree ]

                    expected =
                        Vector.Vector
                            [ complexNumberOne
                            , complexNumberTwo
                            , complexNumberThree
                            ]

                    listOfMonoids =
                        [ c, b, a ]
                in
                Internal.Vector.concat.concat listOfMonoids
                    |> Expect.equal expected
        ]
