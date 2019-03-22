module VectorTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests Vector add is commutative" <|
            \one two ->
                let
                    v =
                        Vector.Vector
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
                        Vector.Vector
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
                Vector.addComplexVectors v w
                    |> Expect.equal (Vector.addComplexVectors w v)
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests Vector add is associative" <|
            \one two three ->
                let
                    v =
                        Vector.Vector
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
                        Vector.Vector
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
                        Vector.Vector
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
                        Vector.addComplexVectors v w
                            |> Vector.addComplexVectors x

                    wPlusXPlusV =
                        Vector.addComplexVectors w x
                            |> Vector.addComplexVectors v
                in
                Vector.addComplexVectors v w
                    |> Expect.equal (Vector.addComplexVectors w v)
        , Test.fuzz2 Fuzz.int Fuzz.int "tests zero is additive identity" <|
            \one two ->
                let
                    v =
                        Vector.sumEmpty [ ComplexNumbers.zero ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                Vector.addComplexVectors v w
                    |> Expect.equal w
        , Test.fuzz2 Fuzz.int Fuzz.int "tests vector inverse" <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.one
                            ]

                    w =
                        Vector.Vector
                            [ ComplexNumbers.negate ComplexNumbers.one
                            ]

                    zero =
                        Vector.Vector
                            [ ComplexNumbers.zero
                            ]
                in
                Vector.addComplexVectors v w
                    |> Expect.equal zero
        , Test.fuzz2 Fuzz.int Fuzz.int "tests one is product identity" <|
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
                Vector.map (ComplexNumbers.multiply ComplexNumbers.one) v
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
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )
                            ]

                    c2V =
                        Vector.map (ComplexNumbers.multiply c2) v

                    c2VThenc1 =
                        Vector.map (ComplexNumbers.multiply c1) c2V

                    c1c2 =
                        ComplexNumbers.multiply c1 c2

                    c1c2ThenV =
                        Vector.map (ComplexNumbers.multiply c1c2) v
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
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    two
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]

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

                    vPlusW =
                        Vector.addComplexVectors v w

                    cvPlusW =
                        Vector.map (ComplexNumbers.multiply c) vPlusW

                    cW =
                        Vector.map (ComplexNumbers.multiply c) w

                    cV =
                        Vector.map (ComplexNumbers.multiply c) v

                    cVPluscW =
                        Vector.addComplexVectors cW cV

                    result =
                        Vector.equal ComplexNumbers.equal cvPlusW cVPluscW
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
                        Vector.Vector
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
                        Vector.map (ComplexNumbers.multiply c1Plusc2) v

                    c1V =
                        Vector.map (ComplexNumbers.multiply c1) v

                    c2V =
                        Vector.map (ComplexNumbers.multiply c2) v

                    c1VPlusc2V =
                        Vector.addComplexVectors c1V c2V

                    result =
                        Vector.equal ComplexNumbers.equal c1VPlusc2V c1Plusc2V
                in
                Expect.true "All elements equal" result
        , Test.fuzz2 Fuzz.float Fuzz.float "tests Vector empty or identity value for sum" <|
            \one two ->
                let
                    v =
                        Vector.Vector
                            [ ComplexNumbers.ComplexNumberCartesian
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )
                            ]
                in
                Monoid.append (Vector.sumComplex [ ComplexNumbers.zero ]) v (Monoid.empty <| Vector.sumComplex [ ComplexNumbers.zero ])
                    |> Expect.equal v
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add" <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two) ]

                    b =
                        Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three) ]

                    c =
                        Vector.Vector [ ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three) ]

                    expected =
                        Vector.addComplexVectors (Vector.addComplexVectors a b) c

                    listOfMonoids =
                        [ a, b, c ]
                in
                Monoid.concat (Vector.sumComplex [ ComplexNumbers.zero ]) listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz Fuzz.int "tests dot product is nondegenerative" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.realVectorDotProduct a a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests dot product respects addition" <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    c =
                        Vector.Vector [ three ]

                    aPlusBDotc =
                        Vector.realVectorDotProduct (Vector.addRealVectors a b) c

                    aDotB =
                        Vector.realVectorDotProduct a c

                    bDotC =
                        Vector.realVectorDotProduct b c

                    aDotBPlusbDotC =
                        aDotB + bDotC
                in
                aPlusBDotc
                    |> Expect.equal aDotBPlusbDotC
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests dot product respects scalar multiplication" <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    threeTimesADotB =
                        Vector.realVectorDotProduct (Vector.map ((*) three) a) b

                    aDotBTimesThree =
                        Vector.realVectorDotProduct a b * three
                in
                threeTimesADotB
                    |> Expect.equal aDotBTimesThree
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests dot product is symetric" <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    aDotB =
                        Vector.realVectorDotProduct a b

                    bDotA =
                        Vector.realVectorDotProduct b a
                in
                aDotB
                    |> Expect.equal bDotA
        , Test.fuzz (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests vector length equals square of dot product" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    squareRootADotA =
                        Basics.sqrt (Vector.realVectorDotProduct a a)

                    aLength =
                        Vector.realLength a
                in
                squareRootADotA
                    |> Expect.equal aLength
        ]
