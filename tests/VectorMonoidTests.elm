module VectorMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Monoid
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
                Monoid.append Vector.concat v (Monoid.empty Vector.concat)
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
                Monoid.append Vector.concat (Monoid.empty Vector.concat) v
                    |> Expect.equal v
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add" <|
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
                        [ a, b, c ]
                in
                Monoid.concat Vector.concat listOfMonoids
                    |> Expect.equal expected
        ]
