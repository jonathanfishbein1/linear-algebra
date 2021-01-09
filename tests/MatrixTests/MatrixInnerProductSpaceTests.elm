module MatrixTests.MatrixInnerProductSpaceTests exposing (suite)

import Expect
import Field
import Fuzz
import Matrix
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
                        Expect.fail err
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
                        Matrix.dotProduct Vector.realInnerProductSpace (Matrix.add Field.float a b) c

                    aDotB =
                        Matrix.dotProduct Vector.realInnerProductSpace a c

                    bDotC =
                        Matrix.dotProduct Vector.realInnerProductSpace b c

                    aDotBPlusbDotC =
                        Result.map2
                            (+)
                            aDotB
                            bDotC
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
                        Matrix.dotProduct Vector.realInnerProductSpace (Matrix.scalarMultiplication Field.float three a) b

                    aDotBTimesThree =
                        Result.map2
                            (*)
                            (Matrix.dotProduct Vector.realInnerProductSpace a b)
                            (Ok three)
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
                        Matrix.normReal a
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
                        Matrix.normReal (Matrix.add Field.float a b)

                    lengthAPlusLengthB =
                        Result.map2
                            (+)
                            (Matrix.normReal a)
                            (Matrix.normReal b)
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
            Fuzz.float
            Fuzz.float
            "tests matrix norm respects scalar multiplication"
          <|
            \one two ->
                let
                    a =
                        Matrix.Matrix <| [ Matrix.RowVector <| Vector.Vector [ one ] ]

                    legnthOfTwoTimesA =
                        Matrix.normReal (Matrix.scalarMultiplication Field.float two a)

                    lengthOfATimesTwo =
                        Result.map
                            ((*) two >> Basics.abs)
                            (Matrix.normReal a)
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
