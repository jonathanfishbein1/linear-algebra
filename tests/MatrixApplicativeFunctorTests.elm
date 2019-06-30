module MatrixApplicativeFunctorTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Applicative Functor abstraction for Matrix"
        [ Test.fuzz Fuzz.int "tests first applicative law for Matrix" <|
            \one ->
                let
                    mIdentity =
                        Matrix.pure identity

                    m =
                        Matrix.pure one

                    mApplied =
                        Matrix.apply mIdentity m
                in
                Expect.equal mApplied m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests second applicative law for Matrix" <|
            \one two three ->
                let
                    f =
                        (<<)

                    fPure =
                        Matrix.pure f

                    u =
                        Matrix.pure identity

                    v =
                        Matrix.pure identity

                    w =
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ 0 ] ]

                    leftSide =
                        Matrix.apply (Matrix.apply (Matrix.apply fPure u) v) w

                    rightSide =
                        Matrix.apply u (Matrix.apply v w)
                in
                Expect.equal leftSide rightSide
        , Test.fuzz Fuzz.int "tests third applicative law for Matrix" <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        Matrix.pure f

                    pureOne =
                        Matrix.pure one

                    mApplied =
                        Matrix.apply pureF pureOne
                in
                Expect.equal mApplied (Matrix.pure <| f one)
        , Test.fuzz Fuzz.int "tests fourth applicative law for Matrix" <|
            \one ->
                let
                    pureOne =
                        Matrix.pure identity

                    pureTwo =
                        Matrix.pure one

                    leftSide =
                        Matrix.apply pureOne pureTwo

                    rightSide =
                        Matrix.apply (Matrix.pure <| Basics.always one) pureOne
                in
                Expect.equal leftSide rightSide
        ]
