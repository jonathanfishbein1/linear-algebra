module VectorTests.InnerProductSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Parser
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Inner Product Vector Space"
        [ Test.fuzz
            (Fuzz.map Basics.toFloat Fuzz.int)
            "tests dot product is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.vectorDotProduct Field.realField a a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests dot product respects addition"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    c =
                        Vector.Vector [ three ]

                    aPlusBDotc =
                        Vector.vectorDotProduct
                            Field.realField
                            (Vector.addVectors Field.realField a b)
                            c

                    aDotB =
                        Vector.vectorDotProduct Field.realField a c

                    bDotC =
                        Vector.vectorDotProduct Field.realField b c

                    aDotBPlusbDotC =
                        aDotB + bDotC
                in
                Expect.within (Expect.Absolute 0.000000001) aDotBPlusbDotC aPlusBDotc
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests dot product respects scalar multiplication"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    threeTimesADotB =
                        Vector.vectorDotProduct
                            Field.realField
                            (Vector.scalarMultiplication Field.realField three a)
                            b

                    aDotBTimesThree =
                        Vector.vectorDotProduct Field.realField a b * three
                in
                Expect.within (Expect.Absolute 0.000000001) aDotBTimesThree threeTimesADotB
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests dot product is symetric"
          <|
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
                Expect.within (Expect.Absolute 0.000000001) bDotA aDotB
        , Test.fuzz
            (Fuzz.floatRange -10 10)
            "tests vector length equals square of dot product"
          <|
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
                    |> Expect.within (Expect.Absolute 0.000000001) aLength
        , Test.fuzz
            Fuzz.float
            "tests vector length is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.vectorLength Field.realField a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests vector length satisfies triangle inequality"
          <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    aPlusBLength =
                        Vector.addVectors
                            Field.realField
                            a
                            b
                            |> Vector.vectorLength
                                Field.realField

                    lengthAPlusLengthB =
                        Vector.vectorLength
                            Field.realField
                            a
                            + Vector.vectorLength
                                Field.realField
                                b
                in
                aPlusBLength
                    |> Expect.atMost lengthAPlusLengthB
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests vector length respects scalar multiplication"
          <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    legnthOfTwoTimesA =
                        Vector.vectorLength
                            Field.realField
                            (Vector.scalarMultiplication Field.realField two a)

                    lengthOfATimesTwo =
                        Basics.abs two * Vector.vectorLength Field.realField a
                in
                legnthOfTwoTimesA
                    |> Expect.within (Expect.Absolute 0.000000001) lengthOfATimesTwo
        , Test.fuzz
            Fuzz.float
            "tests distance is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.distance Vector.realVectorAbelianGroup a a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests vector distance satisfies triangle inequality"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    c =
                        Vector.Vector [ three ]

                    distanceAB =
                        Vector.distance Vector.realVectorAbelianGroup a b

                    distanceAC =
                        Vector.distance Vector.realVectorAbelianGroup a c

                    distanceCB =
                        Vector.distance Vector.realVectorAbelianGroup c b
                in
                distanceAB
                    |> Expect.atMost (distanceAC + distanceCB)
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests distance is symetric"
          <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    distanceAB =
                        Vector.vectorDotProduct
                            Field.realField
                            a
                            b

                    distanceBA =
                        Vector.vectorDotProduct
                            Field.realField
                            b
                            a
                in
                distanceAB
                    |> Expect.within (Expect.Absolute 0.000000001) distanceBA
        ]
