module VectorTests.InnerProductSpaceTests exposing (suite)

import Expect
import Field
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Inner Product Vector Space"
        [ Test.fuzz
            Fuzz.float
            "tests dot product is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.dotProduct Field.realField a a
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
                        Vector.dotProduct
                            Field.realField
                            (Vector.addVectors Field.realField a b)
                            c

                    aDotC =
                        Vector.dotProduct Field.realField a c

                    bDotC =
                        Vector.dotProduct Field.realField b c

                    aDotCPlusBDotC =
                        aDotC + bDotC
                in
                Expect.within (Expect.Absolute 0.000000001) aDotCPlusBDotC aPlusBDotc
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
                        Vector.dotProduct
                            Field.realField
                            (Vector.scalarMultiplication Field.realField three a)
                            b

                    aDotBTimesThree =
                        Vector.dotProduct Field.realField a b * three
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
                        Vector.dotProduct Field.realField a b

                    bDotA =
                        Vector.dotProduct Field.realField b a
                in
                Expect.within (Expect.Absolute 0.000000001) bDotA aDotB
        , Test.fuzz
            (Fuzz.floatRange -10 10)
            "tests vector length equals square root of dot product"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    squareRootADotA =
                        Basics.sqrt (Vector.dotProduct Field.realField a a)

                    aLength =
                        Vector.length Field.realField a
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
                        Vector.length Field.realField a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests vector dot product satisfies Cauchy-Shwartz inequality"
          <|
            \one two ->
                let
                    x =
                        Vector.Vector [ one ]

                    y =
                        Vector.Vector [ two ]

                    absXDotY =
                        Vector.dotProduct
                            Field.realField
                            x
                            y
                            |> Basics.abs

                    lengthOfX =
                        Vector.length Field.realField x

                    lengthOfY =
                        Vector.length Field.realField y

                    lengthOfXTimesLengthOfY =
                        lengthOfX * lengthOfY
                in
                absXDotY
                    |> Expect.atMost lengthOfXTimesLengthOfY
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
                            |> Vector.length
                                Field.realField

                    lengthAPlusLengthB =
                        Vector.length
                            Field.realField
                            a
                            + Vector.length
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
                        Vector.length
                            Field.realField
                            (Vector.scalarMultiplication Field.realField two a)

                    lengthOfATimesTwo =
                        Basics.abs two * Vector.length Field.realField a
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
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
            (Fuzz.map toFloat Fuzz.int)
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
                        Vector.dotProduct
                            Field.realField
                            a
                            b

                    distanceBA =
                        Vector.dotProduct
                            Field.realField
                            b
                            a
                in
                distanceAB
                    |> Expect.within (Expect.Absolute 0.000000001) distanceBA
        , Test.test
            "tests angle between orthagonal vectors"
          <|
            \_ ->
                let
                    a =
                        Vector.Vector [ 1, 0 ]

                    b =
                        Vector.Vector [ 0, 1 ]

                    angle =
                        Vector.angleBetween a b
                in
                angle
                    |> Expect.within (Expect.Absolute 0.000000001) (Basics.pi / 2)
        , Test.test
            "tests angle between colinear vectors"
          <|
            \_ ->
                let
                    a =
                        Vector.Vector [ 1, 0 ]

                    b =
                        Vector.Vector [ 1, 0 ]

                    angle =
                        Vector.angleBetween a b
                in
                angle
                    |> Expect.within (Expect.Absolute 0.000000001) 0
        ]
