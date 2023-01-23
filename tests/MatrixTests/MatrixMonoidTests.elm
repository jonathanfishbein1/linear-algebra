module MatrixTests.MatrixMonoidTests exposing (suite)

import Expect
import Fuzz
import Matrix
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz
            Fuzz.niceFloat
            "tests Matrix empty or identity value for vertical append right"
          <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Matrix.concatVertical.semigroup.prepend m Matrix.concatVertical.identity
                    |> Expect.equal m
        , Test.fuzz
            Fuzz.niceFloat
            "tests Matrix empty or identity value for vertical append left"
          <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Matrix.concatVertical.semigroup.prepend Matrix.concatVertical.identity m
                    |> Expect.equal m
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests monoidally concat matricies vertically"
          <|
            \one two three ->
                let
                    a =
                        RowVector.RowVector <| Vector.Vector [ one ]

                    b =
                        RowVector.RowVector <| Vector.Vector [ two ]

                    c =
                        RowVector.RowVector <| Vector.Vector [ three ]

                    m1 =
                        Matrix.Matrix [ a ]

                    m2 =
                        Matrix.Matrix [ b ]

                    m3 =
                        Matrix.Matrix [ c ]

                    expected =
                        Matrix.Matrix
                            [ c
                            , b
                            , a
                            ]

                    listOfMonoids =
                        [ m1
                        , m2
                        , m3
                        ]
                in
                Matrix.concatVertical.concat listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz
            Fuzz.niceFloat
            "tests Matrix empty or identity value for horizontal append right"
          <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Matrix.concatHorizontal.semigroup.prepend m Matrix.concatHorizontal.identity
                    |> Expect.equal m
        , Test.fuzz
            Fuzz.niceFloat
            "tests Matrix empty or identity value for horizontal append left"
          <|
            \one ->
                let
                    m =
                        Matrix.Matrix <|
                            [ RowVector.RowVector <|
                                Vector.Vector
                                    [ one
                                    ]
                            ]
                in
                Matrix.concatHorizontal.semigroup.prepend Matrix.concatHorizontal.identity m
                    |> Expect.equal m
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests monoidally concat matricies horizontally"
          <|
            \one two three ->
                let
                    a =
                        RowVector.RowVector <| Vector.Vector [ one ]

                    b =
                        RowVector.RowVector <| Vector.Vector [ two ]

                    c =
                        RowVector.RowVector <| Vector.Vector [ three ]

                    m1 =
                        Matrix.Matrix [ a ]

                    m2 =
                        Matrix.Matrix [ b ]

                    m3 =
                        Matrix.Matrix [ c ]

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ three, two, one ]
                            ]

                    listOfMonoids =
                        [ m1, m2, m3 ]
                in
                Matrix.concatHorizontal.concat listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests monoidally concat matricies horizontally with scenerio that might break map2"
          <|
            \one two three ->
                let
                    a =
                        RowVector.RowVector <| Vector.Vector [ one ]

                    b =
                        RowVector.RowVector <| Vector.Vector [ two ]

                    c =
                        RowVector.RowVector <| Vector.Vector [ three ]

                    m1 =
                        Matrix.Matrix
                            [ a
                            , b
                            ]

                    m2 =
                        Matrix.Matrix [ c ]

                    expected =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ three, one ]
                            , RowVector.RowVector <| Vector.Vector [ two ]
                            ]

                    listOfMonoids =
                        [ m1, m2 ]
                in
                Matrix.concatHorizontal.concat listOfMonoids
                    |> Expect.equal expected
        ]
