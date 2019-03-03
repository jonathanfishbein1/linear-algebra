module LinearAlgebraTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import LinearAlgebra
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests Vector add is commutative" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
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
                        LinearAlgebra.Vector
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
                LinearAlgebra.add ComplexNumbers.add v w
                    |> Expect.equal (LinearAlgebra.add ComplexNumbers.add w v)
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Vector add is associative" <|
            \one two three ->
                let
                    v =
                        LinearAlgebra.Vector
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
                        LinearAlgebra.Vector
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
                        LinearAlgebra.Vector
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
                        LinearAlgebra.add ComplexNumbers.add v w
                            |> LinearAlgebra.add ComplexNumbers.add x

                    wPlusXPlusV =
                        LinearAlgebra.add ComplexNumbers.add w x
                            |> LinearAlgebra.add ComplexNumbers.add v
                in
                LinearAlgebra.add ComplexNumbers.add v w
                    |> Expect.equal (LinearAlgebra.add ComplexNumbers.add w v)
        , Test.fuzz2 Fuzz.int Fuzz.int "tests zero is additive identity" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.zero
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                LinearAlgebra.add ComplexNumbers.add v w
                    |> Expect.equal w
        , Test.fuzz2 Fuzz.int Fuzz.int "tests vector inverse" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.one
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.negate ComplexNumbers.one
                            ]

                    zero =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.zero
                            ]
                in
                LinearAlgebra.add ComplexNumbers.add v w
                    |> Expect.equal zero
        , Test.fuzz2 Fuzz.int Fuzz.int "tests one is product identity" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                LinearAlgebra.map (ComplexNumbers.multiply ComplexNumbers.one) v
                    |> Expect.equal v
        , Test.fuzz2 Fuzz.int Fuzz.int "tests scalar multiplication respects complex multiplication" <|
            \one two ->
                let
                    c1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    c2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                two
                            )
                            (ComplexNumbers.Imaginary
                                one
                            )

                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    c2V =
                        LinearAlgebra.map (ComplexNumbers.multiply c2) v

                    c2VThenc1 =
                        LinearAlgebra.map (ComplexNumbers.multiply c1) c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        LinearAlgebra.map (ComplexNumbers.multiply c1c2) v
                in
                c2VThenc1
                    |> Expect.equal c1c2ThenV
        ]
