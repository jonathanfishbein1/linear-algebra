module VectorTests.VectorApplicativeFunctorTests exposing (suite)

import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Applicative Functor abstraction for Vector"
        [ Test.fuzz
            Fuzz.int
            "tests first applicative law for Vector"
          <|
            \one ->
                let
                    vIdentity =
                        Vector.pure identity

                    v =
                        Vector.pure one

                    vApplied =
                        Vector.andMap v vIdentity
                in
                Expect.equal vApplied v
        , Test.fuzz
            Fuzz.int
            "tests second applicative law for Vector"
          <|
            \one ->
                let
                    f =
                        (<<)

                    fPure =
                        Vector.pure f

                    u =
                        Vector.pure identity

                    v =
                        Vector.pure identity

                    w =
                        Vector.Vector [ one ]

                    leftSide =
                        Vector.andMap w (Vector.andMap v (Vector.andMap u fPure))

                    rightSide =
                        Vector.andMap (Vector.andMap w v) u
                in
                Expect.equal leftSide rightSide
        , Test.fuzz
            Fuzz.int
            "tests third applicative law for Vector"
          <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        Vector.pure f

                    pureOne =
                        Vector.pure one

                    vApplied =
                        Vector.andMap pureOne pureF
                in
                Expect.equal vApplied (Vector.pure <| f one)
        , Test.fuzz
            Fuzz.int
            "tests fourth applicative law for Vector"
          <|
            \one ->
                let
                    pureOne =
                        Vector.pure identity

                    pureTwo =
                        Vector.pure one

                    leftSide =
                        Vector.andMap pureTwo pureOne

                    rightSide =
                        Vector.andMap pureOne (Vector.pure <| Basics.always one)
                in
                Expect.equal leftSide rightSide
        ]
