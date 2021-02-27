module VectorTests.VectorApplicativeFunctorTests exposing (suite)

import Expect
import Fuzz
import Internal.Vector
import Test


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
                        Internal.Vector.pure identity

                    v =
                        Internal.Vector.pure one

                    vApplied =
                        Internal.Vector.andMap v vIdentity
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
                        Internal.Vector.pure f

                    u =
                        Internal.Vector.pure identity

                    v =
                        Internal.Vector.pure identity

                    w =
                        Internal.Vector.Vector [ one ]

                    leftSide =
                        Internal.Vector.andMap w (Internal.Vector.andMap v (Internal.Vector.andMap u fPure))

                    rightSide =
                        Internal.Vector.andMap (Internal.Vector.andMap w v) u
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
                        Internal.Vector.pure f

                    pureOne =
                        Internal.Vector.pure one

                    vApplied =
                        Internal.Vector.andMap pureOne pureF
                in
                Expect.equal vApplied (Internal.Vector.pure <| f one)
        , Test.fuzz
            Fuzz.int
            "tests fourth applicative law for Vector"
          <|
            \one ->
                let
                    pureOne =
                        Internal.Vector.pure identity

                    pureTwo =
                        Internal.Vector.pure one

                    leftSide =
                        Internal.Vector.andMap pureTwo pureOne

                    rightSide =
                        Internal.Vector.andMap pureOne (Internal.Vector.pure <| Basics.always one)
                in
                Expect.equal leftSide rightSide
        ]
