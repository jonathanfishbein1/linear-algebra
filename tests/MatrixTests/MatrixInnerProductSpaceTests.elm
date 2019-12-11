module MatrixTests.MatrixInnerProductSpaceTests exposing (suite)

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
    Test.describe "Inner Product Matrix Space"
        [ Test.fuzz
            (Fuzz.map Basics.toFloat Fuzz.int)
            "tests dot product is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    expected =
                        Matrix.dotProduct Vector.realInnerProductSpace a a
                in
                case expected of
                    Ok exp ->
                        exp
                            |> Expect.atLeast 0

                    Err err ->
                        Expect.fail "0 or less"
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests dot product respects addition"
          <|
            \one two three ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    b =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ two ] ]

                    c =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ three ] ]

                    aPlusBDotc =
                        Matrix.dotProduct Vector.realInnerProductSpace (Matrix.addMatrices Field.realField a b) c

                    aDotB =
                        Matrix.dotProduct Vector.realInnerProductSpace a c

                    bDotC =
                        Matrix.dotProduct Vector.realInnerProductSpace b c

                    aDotBPlusbDotC =
                        Result.map2 (+) aDotB bDotC
                in
                aPlusBDotc
                    |> Expect.equal aDotBPlusbDotC
        , Test.fuzz3
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests dot product respects scalar multiplication"
          <|
            \one two three ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    b =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ two ] ]

                    threeTimesADotB =
                        Matrix.dotProduct Vector.realInnerProductSpace (Matrix.scalarMultiplication Field.realField three a) b

                    aDotBTimesThree =
                        Result.map2 (*) (Matrix.dotProduct Vector.realInnerProductSpace a b) (Ok three)
                in
                threeTimesADotB
                    |> Expect.equal aDotBTimesThree
        , Test.fuzz2
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests dot product is symetric"
          <|
            \one two ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    b =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ two ] ]

                    aDotB =
                        Matrix.dotProduct Vector.realInnerProductSpace a b

                    bDotA =
                        Matrix.dotProduct Vector.realInnerProductSpace b a
                in
                aDotB
                    |> Expect.equal bDotA
        , Test.fuzz
            Fuzz.float
            "tests matrix norm is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    expected =
                        Matrix.matrixNorm Vector.realInnerProductSpace a
                in
                case expected of
                    Ok norm ->
                        Expect.atLeast 0 norm

                    Err err ->
                        Expect.fail err
        , Test.fuzz2
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            (Fuzz.map toFloat (Fuzz.intRange -10 10))
            "tests matrix norm satisfies triangle inequality"
          <|
            \one two ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    b =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ two ] ]

                    aPlusBLength =
                        Matrix.matrixNorm Vector.realInnerProductSpace <| Matrix.addMatrices Field.realField a b

                    lengthAPlusLengthB =
                        Result.map2 (+) (Matrix.matrixNorm Vector.realInnerProductSpace a) (Matrix.matrixNorm Vector.realInnerProductSpace b)
                in
                case aPlusBLength of
                    Ok aBLength ->
                        case lengthAPlusLengthB of
                            Ok otherLength ->
                                Expect.atMost otherLength aBLength

                            Err err ->
                                Expect.fail err

                    Err err ->
                        Expect.fail err
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests matrix norm respects scalar multiplication"
          <|
            \one two ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    legnthOfTwoTimesA =
                        Matrix.matrixNorm Vector.realInnerProductSpace (Matrix.scalarMultiplication Field.realField two a)

                    lengthOfATimesTwo =
                        Result.map ((*) two >> Basics.abs) (Matrix.matrixNorm Vector.realInnerProductSpace a)
                in
                case legnthOfTwoTimesA of
                    Ok twoAL ->
                        case lengthOfATimesTwo of
                            Ok lTimesTwo ->
                                Expect.within (Expect.Absolute 0.1) twoAL lTimesTwo

                            Err err ->
                                Expect.fail err

                    Err err ->
                        Expect.fail err
        ]
