module MatrixTests.MatrixApplicativeFunctorTests exposing (suite)

import Expect
import Fuzz
import Internal.Vector
import Matrix
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Applicative Functor abstraction for Matrix"
        [ Test.fuzz
            Fuzz.int
            "tests first applicative law for Matrix"
          <|
            \one ->
                let
                    mIdentity =
                        Matrix.pure identity

                    m =
                        Matrix.pure one

                    mApplied =
                        Matrix.andMap m mIdentity
                in
                Expect.equal mApplied m
        , Test.fuzz
            Fuzz.int
            "tests second applicative law for Matrix"
          <|
            \one ->
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
                        Matrix.Matrix [ RowVector.RowVector <| Vector.Vector [ one ] ]

                    leftSide =
                        Matrix.andMap w (Matrix.andMap v (Matrix.andMap u fPure))

                    rightSide =
                        Matrix.andMap (Matrix.andMap w v) u
                in
                Expect.equal leftSide rightSide
        , Test.fuzz
            Fuzz.int
            "tests third applicative law for Matrix"
          <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        Matrix.pure f

                    pureOne =
                        Matrix.pure one

                    mApplied =
                        Matrix.andMap pureOne pureF
                in
                Expect.equal mApplied (Matrix.pure <| f one)
        , Test.fuzz
            Fuzz.int
            "tests fourth applicative law for Matrix"
          <|
            \one ->
                let
                    pureOne =
                        Matrix.pure identity

                    pureTwo =
                        Matrix.pure one

                    leftSide =
                        Matrix.andMap pureTwo pureOne

                    rightSide =
                        Matrix.andMap pureOne (Matrix.pure <| Basics.always one)
                in
                Expect.equal leftSide rightSide
        ]
