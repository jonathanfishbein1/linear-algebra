module MatrixTests.SquareMatrixTests exposing (suite)

import Expect
import Fuzz
import Matrix
import Real
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
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    isRightStochastic =
                        SquareMatrix.isRightStochastic matrix
                in
                Expect.equal (Ok matrix) isRightStochastic
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
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    isLeftStochastic =
                        SquareMatrix.isLeftStochastic matrix
                in
                Expect.equal (Ok matrix) isLeftStochastic
        , Test.fuzz
            (Fuzz.map Basics.toFloat Fuzz.int)
            "tests dot product is nondegenerative"
          <|
            \one ->
                let
                    a =
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    expected =
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace a a
                in
                case expected of
                    Ok (Real.Real exp) ->
                        exp
                            |> Expect.atLeast 0

                    Err err ->
                        Expect.fail err
        , Test.fuzz3
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
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
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace (SquareMatrix.add Real.field a b) c

                    aDotB =
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace a c

                    bDotC =
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace b c

                    aDotBPlusbDotC =
                        Result.map2
                            Real.add
                            aDotB
                            bDotC
                in
                aPlusBDotc
                    |> Expect.equal aDotBPlusbDotC
        , Test.fuzz3
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
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
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace (SquareMatrix.scalarMultiplication Real.field three a) b

                    aDotBTimesThree =
                        Result.map2
                            Real.multiply
                            (SquareMatrix.dotProduct RowVector.realInnerProductSpace a b)
                            (Ok three)
                in
                threeTimesADotB
                    |> Expect.equal aDotBTimesThree
        , Test.fuzz2
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
            (Fuzz.map (toFloat >> Real.Real) (Fuzz.intRange -10 10))
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
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace a b

                    bDotA =
                        SquareMatrix.dotProduct RowVector.realInnerProductSpace b a
                in
                aDotB
                    |> Expect.equal bDotA
        , Test.fuzz
            Fuzz.niceFloat
            "tests matrix norm is nondegenerative"
          <|
            \one ->
                let
                    a =
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    expected =
                        SquareMatrix.normReal a
                in
                case expected of
                    Ok (Real.Real norm) ->
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
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    b =
                        [ RowVector.RowVector <| Vector.Vector [ two ] ]
                            |> Matrix.Matrix
                            |> Matrix.map Real.Real
                            |> SquareMatrix.SquareMatrix

                    aPlusBLength =
                        SquareMatrix.normReal (SquareMatrix.add Real.field a b)

                    lengthAPlusLengthB =
                        Result.map2
                            Real.add
                            (SquareMatrix.normReal a)
                            (SquareMatrix.normReal b)
                in
                case aPlusBLength of
                    Ok (Real.Real aBLength) ->
                        case lengthAPlusLengthB of
                            Ok (Real.Real otherLength) ->
                                Expect.atMost otherLength aBLength

                            Err err ->
                                Expect.fail err

                    Err err ->
                        Expect.fail err
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix norm respects scalar multiplication"
          <|
            \one two ->
                let
                    a =
                        [ RowVector.RowVector <| Vector.Vector [ one ] ]
                            |> Matrix.Matrix
                            |> SquareMatrix.SquareMatrix

                    legnthOfTwoTimesA =
                        SquareMatrix.normReal (SquareMatrix.scalarMultiplication Real.field two a)
                            |> Result.map Real.real

                    lengthOfATimesTwo =
                        Result.map
                            (Real.multiply two >> Real.real >> Basics.abs)
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
