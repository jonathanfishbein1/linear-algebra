module VectorTests.VectorFunctorTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Tests Functor abstraction for Vector"
        [ Test.fuzz Fuzz.int "tests Vector add is commutative" <|
            \one ->
                let
                    v =
                        Vector.Vector [ one ]

                    vPrime =
                        Vector.map identity v
                in
                Expect.equal v vPrime
        , Test.fuzz Fuzz.int "tests Vector Functor composition" <|
            \one ->
                let
                    v =
                        Vector.Vector [ one ]

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        Vector.map fdotG v
                in
                mapResult
                    |> Expect.equal (Vector.map f (Vector.map g v))
        ]
