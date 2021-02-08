module MatrixTests.SquareMatrixTests exposing (suite)

import DoublyStochasticMatrix exposing (DoublyStochasticMatrix(..))
import Expect
import Field
import Fuzz
import Matrix
import RowVector
import SquareMatrix
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Matrix module"
        [ Test.test
            "tests right stochastic"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> SquareMatrix.SquareMatrix

                    isRightStochastic =
                        SquareMatrix.isRightStochastic matrix
                in
                Expect.true "Is Right Stochastic" isRightStochastic
        , Test.test
            "tests left stochastic"
          <|
            \_ ->
                let
                    matrix =
                        Matrix.Matrix
                            [ RowVector.RowVector <| Vector.Vector [ 0, 1 / 6, 5 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 1 / 3, 1 / 2, 1 / 6 ]
                            , RowVector.RowVector <| Vector.Vector [ 2 / 3, 1 / 3, 0 ]
                            ]
                            |> SquareMatrix.SquareMatrix

                    isLeftStochastic =
                        SquareMatrix.isLeftStochastic matrix
                in
                Expect.true "Is Left Stochastic" isLeftStochastic
        , Test.fuzz
            (Fuzz.map Basics.toFloat Fuzz.int)
            "tests dot product is nondegenerative"
          <|
            \one ->
                let
                    a =
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    expected =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace a a
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
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    b =
                        [ RowVector.RowVector <| Vector.Vector [ two ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    c =
                        [ RowVector.RowVector <| Vector.Vector [ three ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    aPlusBDotc =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace (SquareMatrix.add Field.float a b) c

                    aDotB =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace a c

                    bDotC =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace b c

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
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    b =
                        [ RowVector.RowVector <| Vector.Vector [ two ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    threeTimesADotB =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace (SquareMatrix.scalarMultiplication Field.float three a) b

                    aDotBTimesThree =
                        Result.map2
                            (*)
                            (SquareMatrix.dotProduct Vector.realInnerProductSpace a b)
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
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    b =
                        [ RowVector.RowVector <| Vector.Vector [ two ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    aDotB =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace a b

                    bDotA =
                        SquareMatrix.dotProduct Vector.realInnerProductSpace b a
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
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    expected =
                        SquareMatrix.normReal a
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
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    b =
                        [ RowVector.RowVector <| Vector.Vector [ two ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    aPlusBLength =
                        SquareMatrix.normReal (SquareMatrix.add Field.float a b)

                    lengthAPlusLengthB =
                        Result.map2
                            (+)
                            (SquareMatrix.normReal a)
                            (SquareMatrix.normReal b)
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
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    legnthOfTwoTimesA =
                        SquareMatrix.normReal (SquareMatrix.scalarMultiplication Field.float two a)

                    lengthOfATimesTwo =
                        Result.map
                            ((*) two >> Basics.abs)
                            (SquareMatrix.normReal a)
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
