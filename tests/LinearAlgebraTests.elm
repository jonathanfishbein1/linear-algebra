module LinearAlgebraTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import LinearAlgebra
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests Vector add is commutative" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v w
                    |> Expect.equal (LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add w v)
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Vector add is associative" <|
            \one two three ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    x =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            ]

                    vPlusWPlusX =
                        LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v w
                            |> LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add x

                    wPlusXPlusV =
                        LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add w x
                            |> LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v
                in
                LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v w
                    |> Expect.equal (LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add w v)
        , Test.fuzz2 Fuzz.int Fuzz.int "tests zero is additive identity" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.sumEmpty

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v w
                    |> Expect.equal w
        , Test.fuzz2 Fuzz.int Fuzz.int "tests vector inverse" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.one
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.negate ComplexNumbers.one
                            ]

                    zero =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.zero
                            ]
                in
                LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v w
                    |> Expect.equal zero
        , Test.fuzz2 Fuzz.int Fuzz.int "tests one is product identity" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]
                in
                LinearAlgebra.scalarMultiply (ComplexNumbers.multiply ComplexNumbers.one) v
                    |> Expect.equal v
        , Test.fuzz2 Fuzz.int Fuzz.int "tests scalar multiplication respects complex multiplication" <|
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
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    c2V =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c2) v

                    c2VThenc1 =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c1) c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c1c2) v
                in
                c2VThenc1
                    |> Expect.equal c1c2ThenV
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
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]

                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    vPlusW =
                        LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add v w

                    cvPlusW =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c) vPlusW

                    cW =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c) w

                    cV =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c) v

                    cVPluscW =
                        LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add cW cV

                    result =
                        LinearAlgebra.equal ComplexNumbers.equal cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests scalar multiplication distributes over complex addition" <|
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
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    c1Plusc2 =
                        ComplexNumbers.add c1 c2

                    c1Plusc2V =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c1Plusc2) v

                    c1V =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c1) v

                    c2V =
                        LinearAlgebra.scalarMultiply (ComplexNumbers.multiply c2) v

                    c1VPlusc2V =
                        LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add c1V c2V

                    result =
                        LinearAlgebra.equal ComplexNumbers.equal c1VPlusc2V c1Plusc2V
                in
                Expect.true "All elements equal" result
        , Test.fuzz2 Fuzz.float Fuzz.float "tests Vector empty or identity value for sum" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                Monoid.append (LinearAlgebra.sum ComplexNumbers.zero ComplexNumbers.add) v (Monoid.empty <| LinearAlgebra.sum ComplexNumbers.zero ComplexNumbers.add)
                    |> Expect.equal v
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add" <|
            \one two three ->
                let
                    a =
                        LinearAlgebra.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two) ]

                    b =
                        LinearAlgebra.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three) ]

                    c =
                        LinearAlgebra.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three) ]

                    expected =
                        LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add (LinearAlgebra.add ComplexNumbers.zero ComplexNumbers.add a b) c

                    listOfMonoids =
                        [ a, b, c ]
                in
                Monoid.concat (LinearAlgebra.sum ComplexNumbers.zero ComplexNumbers.add) listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Matrix add is commutative" <|
            \one two three ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            ]

                    m1 =
                        LinearAlgebra.Matrix [ v, w ]

                    m2 =
                        LinearAlgebra.Matrix [ w, v ]
                in
                LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add m1 m2
                    |> Expect.equal (LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add m2 m1)
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Matrix add is associative" <|
            \one two three ->
                let
                    v =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    w =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    three
                                )
                            ]

                    x =
                        LinearAlgebra.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            , ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    three
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]

                    m1 =
                        LinearAlgebra.Matrix [ v, w, x ]

                    m2 =
                        LinearAlgebra.Matrix [ w, v, x ]

                    m3 =
                        LinearAlgebra.Matrix [ x, w, v ]

                    m1Plusm2AndThenPlusm3 =
                        LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add m1 m2
                            |> LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add m3

                    m2Plusm3AndThenm1 =
                        LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add m2 m3
                            |> LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add m1
                in
                m1Plusm2AndThenPlusm3
                    |> Expect.equal m2Plusm3AndThenm1
        , Test.fuzz2 Fuzz.float Fuzz.float "tests Matrix empty or identity value for sum" <|
            \one two ->
                let
                    m =
                        LinearAlgebra.Matrix <|
                            [ LinearAlgebra.Vector
                                [ ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real
                                        one
                                    )
                                    (ComplexNumbers.Imaginary
                                        one
                                    )
                                ]
                            ]
                in
                Monoid.append (LinearAlgebra.sumMatrices ComplexNumbers.zero ComplexNumbers.add) m (Monoid.empty <| LinearAlgebra.sumMatrices ComplexNumbers.zero ComplexNumbers.add)
                    |> Expect.equal m
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add matricies" <|
            \one two three ->
                let
                    a =
                        LinearAlgebra.Matrix [ LinearAlgebra.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two) ] ]

                    b =
                        LinearAlgebra.Matrix [ LinearAlgebra.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three) ] ]

                    c =
                        LinearAlgebra.Matrix [ LinearAlgebra.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three) ] ]

                    expected =
                        LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add (LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add a b) c

                    listOfMonoids =
                        [ a, b, c ]
                in
                Monoid.concat (LinearAlgebra.sumMatrices ComplexNumbers.zero ComplexNumbers.add) listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz2 Fuzz.int Fuzz.int "tests matrix inverse" <|
            \one two ->
                let
                    v =
                        LinearAlgebra.Matrix
                            [ LinearAlgebra.Vector
                                [ ComplexNumbers.one
                                ]
                            ]

                    w =
                        LinearAlgebra.Matrix
                            [ LinearAlgebra.Vector
                                [ ComplexNumbers.negate ComplexNumbers.one
                                ]
                            ]

                    zero =
                        LinearAlgebra.Matrix
                            [ LinearAlgebra.Vector
                                [ ComplexNumbers.zero
                                ]
                            ]
                in
                LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add v w
                    |> Expect.equal zero
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
                        LinearAlgebra.Matrix
                            [ LinearAlgebra.Vector
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
                        LinearAlgebra.Matrix
                            [ LinearAlgebra.Vector
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
                        LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add v w

                    cvPlusW =
                        LinearAlgebra.scalarMatrixMultiply (ComplexNumbers.multiply c) vPlusW

                    cW =
                        LinearAlgebra.scalarMatrixMultiply (ComplexNumbers.multiply c) w

                    cV =
                        LinearAlgebra.scalarMatrixMultiply (ComplexNumbers.multiply c) v

                    cVPluscW =
                        LinearAlgebra.addMatrices ComplexNumbers.zero ComplexNumbers.add cW cV

                    result =
                        LinearAlgebra.equalMatrix ComplexNumbers.equal cvPlusW cVPluscW
                in
                Expect.true "All elements equal" result
        ]
