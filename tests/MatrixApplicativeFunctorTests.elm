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
                        Matrix.Matrix [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    mApplied =
                        Matrix.apply mIdentity m
                in
                Expect.equal mApplied m
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
        ]
