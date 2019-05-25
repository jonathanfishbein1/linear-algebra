module MatrixMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Float.Extra
import Fuzz
import List.Extra
import Matrix
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests Matrix empty or identity value for vertical append" <|
            \one two ->
                let
                    m =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Monoid.append Matrix.matrixConcatVertical m (Monoid.empty Matrix.matrixConcatVertical)
                    |> Expect.equal m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add matricies" <|
            \one two three ->
                let
                    a =
                        Matrix.RowVector <| Vector.Vector [ one ]

                    b =
                        Matrix.RowVector <| Vector.Vector [ two ]

                    c =
                        Matrix.RowVector <| Vector.Vector [ three ]

                    m1 =
                        Matrix.Matrix [ a ]

                    m2 =
                        Matrix.Matrix [ b ]

                    m3 =
                        Matrix.Matrix [ c ]

                    expected =
                        Matrix.Matrix [ a, b, c ]

                    listOfMonoids =
                        [ m1, m2, m3 ]
                in
                Monoid.concat Matrix.matrixConcatVertical listOfMonoids
                    |> Expect.equal expected
        ]
