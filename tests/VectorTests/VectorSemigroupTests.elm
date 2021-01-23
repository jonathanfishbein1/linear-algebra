module VectorTests.VectorSemigroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Vector Abelian Group Tests"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests Vector add is commutative"
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
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                Vector.add ComplexNumbers.complexField v w
                    |> Expect.equal (Vector.add ComplexNumbers.complexField w v)
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests Vector add is associative"
          <|
            \one two three ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    x =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            , ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            ]

                    vPlusWPlusX =
                        Vector.add ComplexNumbers.complexField v w
                            |> Vector.add ComplexNumbers.complexField x

                    wPlusXPlusV =
                        Vector.add ComplexNumbers.complexField w x
                            |> Vector.add ComplexNumbers.complexField v
                in
                Expect.true "vectors sums equal" (Vector.equal ComplexNumbers.equal vPlusWPlusX wPlusXPlusV)
        ]
