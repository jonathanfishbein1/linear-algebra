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
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests second applicative law for Vector" <|
            \one two three ->
                let
                    f =
                        (<<)

                    fPure =
                        Vector.pure f

                    u =
                        Vector.Vector [ identity ]

                    v =
                        Vector.Vector [ identity ]

                    w =
                        Vector.Vector [ 0 ]

                    leftSide =
                        Vector.apply (Vector.apply (Vector.apply fPure u) v) w

                    rightSide =
                        Vector.apply u (Vector.apply v w)
                in
                Expect.equal leftSide rightSide
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
