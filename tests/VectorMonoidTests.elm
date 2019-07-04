module VectorMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests Vector empty or identity value right" <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                Vector.concat.semigroup.prepend v Vector.concat.identity
                    |> Expect.equal v
        , Test.fuzz2 Fuzz.float Fuzz.float "tests Vector empty or identity value left" <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                Vector.concat.semigroup.prepend Vector.concat.identity v
                    |> Expect.equal v
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally concat complex vectors" <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    complexNumberThree =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three)

                    a =
                        Vector.Vector [ complexNumberOne ]

                    b =
                        Vector.Vector [ complexNumberTwo ]

                    c =
                        Vector.Vector [ complexNumberThree ]

                    expected =
                        Vector.Vector [ complexNumberOne, complexNumberTwo, complexNumberThree ]

                    listOfMonoids =
                        [ c, b, a ]
                in
                Vector.concat.concat listOfMonoids
                    |> Expect.equal expected
        ]
