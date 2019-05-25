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
        [ Test.fuzz Fuzz.float "tests Matrix empty or identity value for vertical append right" <|
            \one ->
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
        , Test.fuzz Fuzz.float "tests Matrix empty or identity value for vertical append left" <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Monoid.append Matrix.matrixConcatVertical (Monoid.empty Matrix.matrixConcatVertical) m
                    |> Expect.equal m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally concat matricies vertically" <|
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
                        Matrix.Matrix
                            [ a
                            , b
                            , c
                            ]

                    listOfMonoids =
                        [ m1
                        , m2
                        , m3
                        ]
                in
                Monoid.concat Matrix.matrixConcatVertical listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz Fuzz.float "tests Matrix empty or identity value for horizontal append right" <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Monoid.append Matrix.matrixConcatHorizontal m (Monoid.empty Matrix.matrixConcatHorizontal)
                    |> Expect.equal m
        , Test.fuzz Fuzz.float "tests Matrix empty or identity value for horizontal append left" <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Monoid.append Matrix.matrixConcatHorizontal (Monoid.empty Matrix.matrixConcatHorizontal) m
                    |> Expect.equal m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally concat matricies horizontally" <|
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
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, two, three ]
                            ]

                    listOfMonoids =
                        [ m1, m2, m3 ]
                in
                Monoid.concat Matrix.matrixConcatHorizontal listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally concat matricies horizontally with scenerio that might break map2" <|
            \one two three ->
                let
                    a =
                        Matrix.RowVector <| Vector.Vector [ one ]

                    b =
                        Matrix.RowVector <| Vector.Vector [ two ]

                    c =
                        Matrix.RowVector <| Vector.Vector [ three ]

                    m1 =
                        Matrix.Matrix
                            [ a
                            , b
                            ]

                    m2 =
                        Matrix.Matrix [ c ]

                    expected =
                        Matrix.Matrix
                            [ Matrix.RowVector <| Vector.Vector [ one, three ]
                            , Matrix.RowVector <| Vector.Vector [ two ]
                            ]

                    listOfMonoids =
                        [ m1, m2 ]
                in
                Monoid.concat Matrix.matrixConcatHorizontal listOfMonoids
                    |> Expect.equal expected
        ]
