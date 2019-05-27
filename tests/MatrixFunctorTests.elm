module MatrixFunctorTests exposing (suite)

import Expect
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Functor abstraction for Matrix"
        [ Test.fuzz Fuzz.int "tests Matrix mapped with identity is the same" <|
            \one ->
                let
                    m =
                        Vector.Vector [ one ]
                            |> Matrix.RowVector
                            |> List.singleton
                            |> Matrix.Matrix

                    mPrime =
                        Matrix.map identity m
                in
                Expect.equal m mPrime
        , Test.fuzz Fuzz.int "tests Matrix Functor composition" <|
            \one ->
                let
                    m =
                        Vector.Vector [ one ]
                            |> Matrix.RowVector
                            |> List.singleton
                            |> Matrix.Matrix

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        Matrix.map fdotG m
                in
                mapResult
                    |> Expect.equal (Matrix.map f (Matrix.map g m))
        ]
