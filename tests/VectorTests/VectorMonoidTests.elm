module VectorTests.VectorMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests Vector empty or identity value right"
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
                Expect.true "vectors equal" (Vector.equal ComplexNumbers.equal (Vector.concat.semigroup.prepend v Vector.concat.identity) v)
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
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                Expect.true "vectors equal" (Vector.equal ComplexNumbers.equal (Vector.concat.semigroup.prepend Vector.concat.identity v) v)
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
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real two)
                            (ComplexNumbers.Imaginary three)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary three)

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
