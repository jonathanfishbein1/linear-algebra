module VectorTests.VectorFunctorTests exposing (suite)

import Expect
import Fuzz
import Internal.Vector
import Test


suite : Test.Test
suite =
    Test.describe "Tests Functor abstraction for Vector"
        [ Test.fuzz
            Fuzz.int
            "tests Vector add is commutative"
          <|
            \one ->
                let
                    v =
                        Internal.Vector.Vector [ one ]

                    vPrime =
                        Internal.Vector.map identity v
                in
                Expect.equal v vPrime
        , Test.fuzz
            Fuzz.int
            "tests Vector Functor composition"
          <|
            \one ->
                let
                    v =
                        Internal.Vector.Vector [ one ]

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        Internal.Vector.map fdotG v
                in
                mapResult
                    |> Expect.equal (Internal.Vector.map f (Internal.Vector.map g v))
        ]
