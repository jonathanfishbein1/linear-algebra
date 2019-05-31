module MatrixMonadTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Monad abstraction for Matrix"
        [ Test.fuzz Fuzz.int "tests Matrix Monad left identity" <|
            \one ->
                let
                    f a =
                        [ a * 2 ]
                            |> Vector.Vector
                            |> Matrix.RowVector
                            |> List.singleton
                            |> Matrix.Matrix

                    leftSide =
                        Matrix.bind (Matrix.pure one) f

                    rightSide =
                        f one
                in
                Expect.equal leftSide rightSide
        , Test.fuzz Fuzz.int "tests Matrix Monad right identity" <|
            \one ->
                let
                    m =
                        Matrix.pure one

                    leftSide =
                        Matrix.bind m Matrix.pure
                in
                Expect.equal leftSide m
        , Test.fuzz Fuzz.int "tests Matrix Monad associativity" <|
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
                        Matrix.bind (Matrix.bind m f) g

                    rightSide =
                        Matrix.bind m (\x -> Matrix.bind (f x) g)
                in
                Expect.equal leftSide rightSide
        ]
