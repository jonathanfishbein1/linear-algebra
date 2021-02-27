module VectorTests.VectorMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests empty vector is additive identity"
          <|
            \one two ->
                let
                    w =
                        Internal.Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Internal.Vector.add ComplexNumbers.field (Internal.Vector.Vector [ ComplexNumbers.zero ]) w
                    |> Expect.equal w
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests Vector empty or identity value right"
          <|
            \one two ->
                let
                    v =
                        Internal.Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Expect.true "vectors equal" ((Internal.Vector.equal ComplexNumbers.equal.eq).eq (Internal.Vector.concat.semigroup v Internal.Vector.concat.identity) v)
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests Vector empty or identity value left"
          <|
            \one two ->
                let
                    v =
                        Internal.Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Expect.true "vectors equal" ((Internal.Vector.equal ComplexNumbers.equal.eq).eq (Internal.Vector.concat.semigroup Internal.Vector.concat.identity v) v)
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
                        Internal.Vector.Vector [ complexNumberOne ]

                    b =
                        Internal.Vector.Vector [ complexNumberTwo ]

                    c =
                        Internal.Vector.Vector [ complexNumberThree ]

                    expected =
                        Internal.Vector.Vector
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
