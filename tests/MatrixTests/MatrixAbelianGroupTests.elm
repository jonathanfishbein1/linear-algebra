module MatrixTests.MatrixAbelianGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Matrix
import Parser
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Abelian Group Tests"
        [ Test.fuzz3
            Fuzz.float
            Fuzz.float
            Fuzz.float
            "tests Matrix add is commutative"
          <|
            \one two three ->
                let
                    v =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                , ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                ]

                    w =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        two
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        three
                                    )
                                ]

                    m1 =
                        Matrix.Matrix [ v, w ]

                    m2 =
                        Matrix.Matrix [ w, v ]
                in
                Matrix.add ComplexNumbers.complexField m1 m2
                    |> Expect.equal (Matrix.add ComplexNumbers.complexField m2 m1)
        , Test.fuzz3
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            "tests Matrix add is associative"
          <|
            \one two three ->
                let
                    v =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                , ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                ]

                    w =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        two
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        three
                                    )
                                ]

                    x =
                        Matrix.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumber
                                    (ComplexNumbers.Real
                                        three
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                ]

                    m1 =
                        Matrix.Matrix [ v, w, x ]

                    m2 =
                        Matrix.Matrix [ w, v, x ]

                    m3 =
                        Matrix.Matrix [ x, w, v ]

                    m1Plusm2AndThenPlusm3 =
                        Matrix.add ComplexNumbers.complexField m1 m2
                            |> Matrix.add ComplexNumbers.complexField m3

                    m2Plusm3AndThenm1 =
                        Matrix.add ComplexNumbers.complexField m2 m3
                            |> Matrix.add ComplexNumbers.complexField m1
                in
                m1Plusm2AndThenPlusm3
                    |> Expect.equal m2Plusm3AndThenm1
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests matrix inverse"
          <|
            \one two ->
                let
                    complexOneNegative =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real <|
                                Basics.negate
                                    1
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    v =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.one ]
                            ]

                    w =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ complexOneNegative ]
                            ]

                    zero =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.zero
                                    ]
                            ]
                in
                Matrix.add ComplexNumbers.complexField v w
                    |> Expect.equal zero
        ]
