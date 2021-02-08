module MatrixTests.MatrixSemigroupTests exposing (..)

import ComplexNumbers
import Expect
import Fuzz
import Matrix
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Semigroup Tests"
        [ Test.fuzz3
            Fuzz.float
            Fuzz.float
            Fuzz.float
            "tests Matrix add is commutative"
          <|
            \one two three ->
                let
                    v =
                        RowVector.RowVector <|
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
                        RowVector.RowVector <|
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
                        RowVector.RowVector <|
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
                        RowVector.RowVector <|
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
                        RowVector.RowVector <|
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
        ]
