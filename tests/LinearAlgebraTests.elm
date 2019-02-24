module LinearAlgebraTests exposing (suite)

import Expect
import Fuzz
import LinearAlgebra
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests Vector add is commutative" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector [ one, two ]

                    w =
                        LinearAlgebra.Vector [ one, two ]
                in
                LinearAlgebra.add (+) v w
                    |> Expect.equal (LinearAlgebra.add (+) w v)
        ]
