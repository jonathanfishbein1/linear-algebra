module VectorApplicativeFunctorTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Applicative Functor abstraction for Vector"
        [ Test.fuzz Fuzz.int "tests first applicative law for Vector" <|
            \one ->
                let
                    vIdentity =
                        Vector.pure identity

                    v =
                        Vector.Vector [ one ]

                    vApplied =
                        Vector.apply vIdentity v
                in
                Expect.equal vApplied v
        ]
