module VectorTests.VectorMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests empty vector is additive identity"
          <|
            \one two ->
                let
                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (Real.Real
                                    one
                                )
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Vector.add ComplexNumbers.complexField (Vector.Vector [ ComplexNumbers.zero ]) w
                    |> Expect.equal w
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests Vector empty or identity value right"
          <|
            \one two ->
                let
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
                in
                Expect.true "vectors equal" (Vector.equal ComplexNumbers.equal.eq (Vector.concat.semigroup v Vector.concat.identity) v)
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests Vector empty or identity value left"
          <|
            \one two ->
                let
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
                in
                Expect.true "vectors equal" (Vector.equal ComplexNumbers.equal.eq (Vector.concat.semigroup Vector.concat.identity v) v)
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests monoidally concat complex vectors"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            (Real.Real two)
                            (Imaginary.Imaginary three)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
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
                Vector.concat.concat listOfMonoids
                    |> Expect.equal expected
        ]
