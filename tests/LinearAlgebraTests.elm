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
        ]
