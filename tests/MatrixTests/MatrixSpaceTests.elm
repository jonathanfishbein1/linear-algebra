module MatrixTests.MatrixSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Matrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Matrix Space Tests"
        [ Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests one is product identity" <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                Expect.true "equal" (Vector.equal ComplexNumbers.equal (Vector.map (ComplexNumbers.multiply ComplexNumbers.one) v) v)
        , Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests scalar multiplication respects complex multiplication" <|
            \one two ->
                let
                    c1 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    c2 =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                two
                            )
                            (ComplexNumbers.Imaginary
                                one
                            )

                    v =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    c2V =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c2 v

                    c2VThenc1 =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c1 c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c1c2 v
                in
                c2VThenc1
                    |> Expect.equal c1c2ThenV
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scalar multiplication distributes over addition" <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    w =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    v =
                        Matrix.Matrix
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    vPlusW =
                        Matrix.addMatrices ComplexNumbers.complexField v w

                    cvPlusW =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c vPlusW

                    cW =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c w

                    cV =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c v

                    cVPluscW =
                        Matrix.addMatrices ComplexNumbers.complexField cW cV

                    result =
                        Matrix.equal ComplexNumbers.equal cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests scalar multiplication distributes over addition" <|
            \one two ->
                let
                    c =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    w =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            two
                                        )
                                        (ComplexNumbers.Imaginary
                                            one
                                        )
                                    ]
                            ]

                    v =
                        Matrix.Matrix <|
                            [ Matrix.RowVector <|
                                Vector.Vector
                                    [ ComplexNumbers.ComplexNumberCartesian
                                        (ComplexNumbers.Real
                                            one
                                        )
                                        (ComplexNumbers.Imaginary
                                            two
                                        )
                                    ]
                            ]

                    vPlusW =
                        Matrix.addMatrices ComplexNumbers.complexField v w

                    cvPlusW =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c vPlusW

                    cW =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c w

                    cV =
                        Matrix.scalarMultiplication ComplexNumbers.complexField c v

                    cVPluscW =
                        Matrix.addMatrices ComplexNumbers.complexField cW cV

                    result =
                        Matrix.equal ComplexNumbers.equal cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        ]
