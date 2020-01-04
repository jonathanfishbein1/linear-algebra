module VectorTests.VectorAbelianGroupTests exposing (suite)

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
                Vector.addVectors ComplexNumbers.complexField v w
                    |> Expect.equal (Vector.addVectors ComplexNumbers.complexField w v)
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
                        Vector.addVectors ComplexNumbers.complexField v w
                            |> Vector.addVectors ComplexNumbers.complexField x

                    wPlusXPlusV =
                        Vector.addVectors ComplexNumbers.complexField w x
                            |> Vector.addVectors ComplexNumbers.complexField v
                in
                Expect.true "vectors sums equal" (Vector.equal ComplexNumbers.equal vPlusWPlusX wPlusXPlusV)
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests empty vector is additive identity"
          <|
            \one two ->
                let
                    w =
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
                Vector.addVectors ComplexNumbers.complexField (Vector.Vector [ ComplexNumbers.zero ]) w
                    |> Expect.equal w
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests vector inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    v =
                        Vector.Vector
                            [ complexNumber ]

                    w =
                        Vector.Vector
                            [ complexNumber ]

                    zero =
                        Vector.Vector
                            [ ComplexNumbers.zero ]
                in
                Vector.subtractVectors ComplexNumbers.complexField v w
                    |> Expect.equal zero
        ]
