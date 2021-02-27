module VectorTests.VectorMonadTests exposing (suite)

import Expect
import Fuzz
import Internal.Vector
import Test


suite : Test.Test
suite =
    Test.describe "Tests Monad abstraction for Vector"
        [ Test.fuzz
            Fuzz.int
            "tests Vector Monad left identity"
          <|
            \one ->
                let
                    f a =
                        [ a * 2 ]
                            |> Internal.Vector.Vector

                    leftSide =
                        Internal.Vector.andThen f (Internal.Vector.pure one)

                    rightSide =
                        f one
                in
                Expect.equal leftSide rightSide
        , Test.fuzz
            Fuzz.int
            "tests Vector Monad right identity"
          <|
            \one ->
                let
                    m =
                        Internal.Vector.pure one

                    leftSide =
                        Internal.Vector.andThen Internal.Vector.pure m
                in
                Expect.equal leftSide m
        , Test.fuzz
            Fuzz.int
            "tests Vector Monad associativity"
          <|
            \one ->
                let
                    m =
                        Internal.Vector.pure one

                    f a =
                        [ a * 2 ]
                            |> Internal.Vector.Vector

                    g a =
                        [ a * 3 ]
                            |> Internal.Vector.Vector

                    leftSide =
                        Internal.Vector.andThen g (Internal.Vector.andThen f m)

                    rightSide =
                        Internal.Vector.andThen (\x -> Internal.Vector.andThen g (f x)) m
                in
                Expect.equal leftSide rightSide
        ]
