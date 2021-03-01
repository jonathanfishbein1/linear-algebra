module MatrixTests.MatrixSemigroupTests exposing (..)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Matrix
import Real
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Semigroup Tests"
        [ Test.fuzz3
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests Matrix add is commutative"
          <|
            \one two three ->
                let
                    v =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    three
                                    (Imaginary.Imaginary
                                        one
                                    )
                                , ComplexNumbers.ComplexNumber
                                    three
                                    (Imaginary.Imaginary
                                        two
                                    )
                                ]

                    w =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    two
                                    (Imaginary.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumber
                                    one
                                    (Imaginary.Imaginary
                                        three
                                    )
                                ]

                    m1 =
                        Matrix.Matrix [ v, w ]

                    m2 =
                        Matrix.Matrix [ w, v ]
                in
                Matrix.add ComplexNumbers.field m1 m2
                    |> Expect.equal (Matrix.add ComplexNumbers.field m2 m1)
        , Test.fuzz3
            (Fuzz.map (toFloat >> Real.Real) Fuzz.int)
            (Fuzz.map (toFloat >> Real.Real) Fuzz.int)
            (Fuzz.map (toFloat >> Real.Real) Fuzz.int)
            "tests Matrix add is associative"
          <|
            \one two three ->
                let
                    v =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    three
                                    (Imaginary.Imaginary
                                        one
                                    )
                                , ComplexNumbers.ComplexNumber
                                    three
                                    (Imaginary.Imaginary
                                        two
                                    )
                                ]

                    w =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    two
                                    (Imaginary.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumber
                                    one
                                    (Imaginary.Imaginary
                                        three
                                    )
                                ]

                    x =
                        RowVector.RowVector <|
                            Vector.Vector
                                [ ComplexNumbers.ComplexNumber
                                    one
                                    (Imaginary.Imaginary
                                        two
                                    )
                                , ComplexNumbers.ComplexNumber
                                    three
                                    (Imaginary.Imaginary
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
                        Matrix.add ComplexNumbers.field m1 m2
                            |> Matrix.add ComplexNumbers.field m3

                    m2Plusm3AndThenm1 =
                        Matrix.add ComplexNumbers.field m2 m3
                            |> Matrix.add ComplexNumbers.field m1
                in
                m1Plusm2AndThenPlusm3
                    |> Expect.equal m2Plusm3AndThenm1
        ]
