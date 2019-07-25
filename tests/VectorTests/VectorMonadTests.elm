module VectorTests.VectorMonadTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Monad abstraction for Vector"
        [ Test.fuzz Fuzz.int "tests Vector Monad left identity" <|
            \one ->
                let
                    f a =
                        [ a * 2 ]
                            |> Vector.Vector

                    leftSide =
                        Vector.bind (Vector.pure one) f

                    rightSide =
                        f one
                in
                Expect.equal leftSide rightSide
        , Test.fuzz Fuzz.int "tests Vector Monad right identity" <|
            \one ->
                let
                    m =
                        Vector.pure one

                    leftSide =
                        Vector.bind m Vector.pure
                in
                Expect.equal leftSide m
        , Test.fuzz Fuzz.int "tests Vector Monad associativity" <|
            \one ->
                let
                    m =
                        Vector.pure one

                    f a =
                        [ a * 2 ]
                            |> Vector.Vector

                    g a =
                        [ a * 3 ]
                            |> Vector.Vector

                    leftSide =
                        Vector.bind (Vector.bind m f) g

                    rightSide =
                        Vector.bind m (\x -> Vector.bind (f x) g)
                in
                Expect.equal leftSide rightSide
        ]
