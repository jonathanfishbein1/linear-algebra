module MatrixTests.ComplexInnerProductSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Field
import Parser
import Test
import Vector
import Field 

suite : Test.Test 
suite = 
    Test.describe "Complex Inner Product Vector Space" 
        [ Test.describe "Complex Vector Space"
            [ Test.describe "Abelian Group"
                [ Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests Vector add is commutative" <|
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
                        Vector.addVectors ComplexNumbers.complexField v w
                            |> Expect.equal (Vector.addVectors ComplexNumbers.complexField w v)
                , Test.fuzz3 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests Vector add is associative" <|
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
                                Vector.addVectors ComplexNumbers.complexField v w
                                    |> Vector.addVectors ComplexNumbers.complexField x

                            wPlusXPlusV =
                                Vector.addVectors ComplexNumbers.complexField w x
                                    |> Vector.addVectors ComplexNumbers.complexField v
                        in
                        Vector.addVectors ComplexNumbers.complexField v w
                            |> Expect.equal (Vector.addVectors ComplexNumbers.complexField w v)
                , Test.fuzz2 Fuzz.int Fuzz.int "tests empty vector is additive identity" <|
                    \one two ->
                        let
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
                        Vector.append Vector.concatEmpty w
                            |> Expect.equal w
                , Test.fuzz2 Fuzz.int Fuzz.int "tests vector inverse" <|
                    \one two ->
                        let
                            complexOneNegative =
                                ComplexNumbers.ComplexNumberCartesian
                                    (ComplexNumbers.Real <|
                                        Basics.negate 1
                                    )
                                    (ComplexNumbers.Imaginary
                                        0
                                    )

                            v =
                                Vector.Vector
                                    [ ComplexNumbers.one ]

                            w =
                                Vector.Vector
                                    [ complexOneNegative ]

                            zero =
                                Vector.Vector
                                    [ ComplexNumbers.zero ]
                        in
                        Vector.addVectors ComplexNumbers.complexField v w
                            |> Expect.equal zero
                ]
            , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests one is product identity" <|
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
                            Vector.addVectors ComplexNumbers.complexField v w

                        cvPlusW =
                            Vector.map (ComplexNumbers.multiply c) vPlusW

                        cW =
                            Vector.map (ComplexNumbers.multiply c) w

                        cV =
                            Vector.map (ComplexNumbers.multiply c) v

                        cVPluscW =
                            Vector.addVectors ComplexNumbers.complexField cW cV

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
                            Vector.addVectors ComplexNumbers.complexField c1V c2V

                        result =
                            Vector.equal ComplexNumbers.equal c1VPlusc2V c1Plusc2V
                    in
                    Expect.true "All elements equal" result
            ]
        , Test.fuzz (Fuzz.map Basics.toFloat Fuzz.int) "tests dot product is nondegenerative" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.vectorDotProduct Field.realField a a
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
                        Vector.vectorDotProduct Field.realField (Vector.addVectors Field.realField a b) c

                    aDotB =
                        Vector.vectorDotProduct Field.realField a c

                    bDotC =
                        Vector.vectorDotProduct Field.realField b c

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
                        Vector.vectorDotProduct Field.realField (Vector.map ((*) three) a) b

                    aDotBTimesThree =
                        Vector.vectorDotProduct Field.realField a b * three
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
                        Vector.vectorDotProduct Field.realField a b

                    bDotA =
                        Vector.vectorDotProduct Field.realField b a
                in
                aDotB
                    |> Expect.equal bDotA
        , Test.fuzz (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests vector length equals square of dot product" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    squareRootADotA =
                        Basics.sqrt (Vector.vectorDotProduct Field.realField a a)

                    aLength =
                        Vector.vectorLength Field.realField a
                in
                squareRootADotA
                    |> Expect.equal aLength
        , Test.fuzz Fuzz.float "tests vector length is nondegenerative" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.vectorLength Field.realField a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests vector length satisfies triangle inequality" <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    aPlusBLength =
                        Vector.vectorLength Field.realField <| Vector.addVectors Field.realField a b

                    lengthAPlusLengthB =
                        Vector.vectorLength Field.realField a + Vector.vectorLength Field.realField b
                in
                aPlusBLength
                    |> Expect.atMost lengthAPlusLengthB
        , Test.fuzz2 (Fuzz.floatRange -10 10) (Fuzz.floatRange -10 10) "tests vector length respects scalar multiplication" <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    legnthOfTwoTimesA =
                        Vector.vectorLength Field.realField (Vector.map ((*) two) a)

                    lengthOfATimesTwo =
                        Basics.abs two * Vector.vectorLength Field.realField a
                in
                legnthOfTwoTimesA
                    |> Expect.within (Expect.Absolute 0.1) lengthOfATimesTwo
        , Test.fuzz Fuzz.float "tests distance is nondegenerative" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.distance a a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests vector distance satisfies triangle inequality" <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    c =
                        Vector.Vector [ three ]

                    distanceAB =
                        Vector.distance a b

                    distanceAC =
                        Vector.distance a c

                    distanceCB =
                        Vector.distance c b
                in
                distanceAB
                    |> Expect.atMost (distanceAC + distanceCB)
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests distance is symetric" <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    distanceAB =
                        Vector.vectorDotProduct Field.realField a b

                    distanceBA =
                        Vector.vectorDotProduct Field.realField b a
                in
                distanceAB
                    |> Expect.equal distanceBA
        ]
