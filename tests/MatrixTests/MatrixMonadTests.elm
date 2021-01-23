module MatrixTests.MatrixMonadTests exposing (suite)

import Expect
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Monad abstraction for Matrix"
        [ Test.fuzz
            Fuzz.int
            "tests Matrix Monad left identity"
          <|
            \one ->
                let
                    f a =
                        [ a * 2 ]
                            |> Vector.Vector
                            |> Matrix.RowVector
                            |> List.singleton
                            |> Matrix.Matrix

                    leftSide =
                        Matrix.andThen f (Matrix.pure one)

                    rightSide =
                        f one
                in
                Expect.equal leftSide rightSide
        , Test.fuzz
            Fuzz.int
            "tests Matrix Monad right identity"
          <|
            \one ->
                let
                    m =
                        Matrix.pure one

                    leftSide =
                        Matrix.andThen Matrix.pure m
                in
                Expect.equal leftSide m
        , Test.fuzz
            Fuzz.int
            "tests Matrix Monad associativity"
          <|
            \one ->
                let
                    m =
                        Matrix.pure one

                    f a =
                        [ a * 2 ]
                            |> Vector.Vector
                            |> Matrix.RowVector
                            |> List.singleton
                            |> Matrix.Matrix

                    g a =
                        [ a * 3 ]
                            |> Vector.Vector
                            |> Matrix.RowVector
                            |> List.singleton
                            |> Matrix.Matrix

                    leftSide =
                        Matrix.andThen g (Matrix.andThen f m)

                    rightSide =
                        Matrix.andThen (\x -> Matrix.andThen g (f x)) m
                in
                Expect.equal leftSide rightSide
        ]
