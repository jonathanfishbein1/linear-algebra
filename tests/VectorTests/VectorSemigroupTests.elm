module VectorTests.VectorSemigroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Vector Abelian Group Tests"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests Vector add is commutative"
          <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                two
                                (Imaginary.Imaginary
                                    two
                                )
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                two
                                (Imaginary.Imaginary
                                    two
                                )
                            ]
                in
                Vector.add ComplexNumbers.field v w
                    |> Expect.equal (Vector.add ComplexNumbers.field w v)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests Vector add is associative"
          <|
            \one two three ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                two
                                (Imaginary.Imaginary
                                    two
                                )
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                one
                                (Imaginary.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumber
                                two
                                (Imaginary.Imaginary
                                    two
                                )
                            ]

                    x =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumber
                                three
                                (Imaginary.Imaginary
                                    three
                                )
                            , ComplexNumbers.ComplexNumber
                                three
                                (Imaginary.Imaginary
                                    three
                                )
                            ]

                    vPlusWPlusX =
                        Vector.add ComplexNumbers.field v w
                            |> Vector.add ComplexNumbers.field x

                    wPlusXPlusV =
                        Vector.add ComplexNumbers.field w x
                            |> Vector.add ComplexNumbers.field v
                in
                Expect.true "vectors sums equal" ((Vector.equal ComplexNumbers.equal.eq).eq vPlusWPlusX wPlusXPlusV)
        ]
