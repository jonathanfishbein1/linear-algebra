module VectorTests.VectorAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Parser
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Complex Inner Product Vector Space"
        [ Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests Vector add is commutative" <|
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
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                Vector.addVectors ComplexNumbers.complexField v w
                    |> Expect.equal (Vector.addVectors ComplexNumbers.complexField w v)
        , Test.fuzz3 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests Vector add is associative" <|
            \one two three ->
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
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    x =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            ]

                    vPlusWPlusX =
                        Vector.addVectors ComplexNumbers.complexField v w
                            |> Vector.addVectors ComplexNumbers.complexField x

                    wPlusXPlusV =
                        Vector.addVectors ComplexNumbers.complexField w x
                            |> Vector.addVectors ComplexNumbers.complexField v
                in
                Vector.addVectors ComplexNumbers.complexField v w
                    |> Expect.equal (Vector.addVectors ComplexNumbers.complexField w v)
        , Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests empty vector is additive identity" <|
            \one two ->
                let
                    w =
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
                Vector.addVectors ComplexNumbers.complexField (Vector.Vector [ ComplexNumbers.zero ]) w
                    |> Expect.equal w
        , Test.fuzz2 Fuzz.int Fuzz.int "tests vector inverse" <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.one ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.one ]

                    zero =
                        Vector.Vector
                            [ ComplexNumbers.zero ]
                in
                Vector.subtractVectors ComplexNumbers.complexField v w
                    |> Expect.equal zero
        ]
