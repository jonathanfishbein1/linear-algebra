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
        , Test.fuzz Fuzz.int "tests third applicative law for Vector" <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        Vector.pure f

                    pureOne =
                        Vector.pure one

                    vApplied =
                        Vector.apply pureF pureOne
                in
                Expect.equal vApplied (Vector.pure <| f one)
        ]
