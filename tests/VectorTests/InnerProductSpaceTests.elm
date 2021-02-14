module VectorTests.InnerProductSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Fuzz
import Imaginary
import Real
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
                        Vector.dotProduct Field.float a a
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
                            Field.float
                            (Vector.add Field.float a b)
                            c

                    aDotC =
                        Vector.dotProduct Field.float a c

                    bDotC =
                        Vector.dotProduct Field.float b c

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
                            Field.float
                            (Vector.scalarMultiplication Field.float three a)
                            b

                    aDotBTimesThree =
                        Vector.dotProduct Field.float a b * three
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
                        Vector.dotProduct Field.float a b

                    bDotA =
                        Vector.dotProduct Field.float b a
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
                        Basics.sqrt (Vector.dotProduct Field.float a a)

                    aLength =
                        Vector.lengthReal a
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
                        Vector.lengthReal a
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
                            Field.float
                            x
                            y
                            |> Basics.abs

                    lengthOfX =
                        Vector.lengthReal x

                    lengthOfY =
                        Vector.lengthReal y

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
                        Vector.add
                            Field.float
                            a
                            b
                            |> Vector.lengthReal

                    lengthAPlusLengthB =
                        Vector.lengthReal
                            a
                            + Vector.lengthReal
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
                        Vector.lengthReal
                            (Vector.scalarMultiplication Field.float two a)

                    lengthOfATimesTwo =
                        Basics.abs two * Vector.lengthReal a
                in
                legnthOfTwoTimesA
                    |> Expect.within (Expect.Absolute 0.000000001) lengthOfATimesTwo
        , Test.fuzz
            (Fuzz.floatRange -10 10)
            "tests complex vector length equals square root of dot product"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber (Real.Real one) (Imaginary.Imaginary one)

                    a =
                        Vector.Vector [ complexNumber ]

                    squareRootADotA =
                        Vector.dotProduct ComplexNumbers.field a (Vector.conjugate a)
                            |> ComplexNumbers.real
                            |> Basics.sqrt

                    aLength =
                        Vector.lengthComplex a
                in
                squareRootADotA
                    |> Expect.within (Expect.Absolute 0.000000001) aLength
        , Test.fuzz
            Fuzz.float
            "tests complex vector length is nondegenerative"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber (Real.Real one) (Imaginary.Imaginary one)

                    a =
                        Vector.Vector [ complexNumber ]

                    expected =
                        Vector.lengthComplex a
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests complex vector dot product satisfies Cauchy-Shwartz inequality"
          <|
            \one two ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber (Real.Real one) (Imaginary.Imaginary one)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber (Real.Real two) (Imaginary.Imaginary two)

                    x =
                        Vector.Vector [ complexNumberOne ]

                    y =
                        Vector.Vector [ complexNumberTwo ]

                    absXDotY =
                        Vector.dotProduct
                            ComplexNumbers.field
                            x
                            y
                            |> ComplexNumbers.real
                            |> Basics.abs

                    lengthOfX =
                        Vector.lengthComplex x

                    lengthOfY =
                        Vector.lengthComplex y

                    lengthOfXTimesLengthOfY =
                        lengthOfX * lengthOfY
                in
                absXDotY
                    |> Expect.atMost lengthOfXTimesLengthOfY
        , Test.fuzz
            Fuzz.float
            "tests distance is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.distanceReal a a
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
                        Vector.distanceReal a b

                    distanceAC =
                        Vector.distanceReal a c

                    distanceCB =
                        Vector.distanceReal c b
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
                            Field.float
                            a
                            b

                    distanceBA =
                        Vector.dotProduct
                            Field.float
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
        , Test.test
            "tests angle between colinear but opposite vectors"
          <|
            \_ ->
                let
                    a =
                        Vector.Vector [ 1, 0 ]

                    b =
                        Vector.Vector [ -1, 0 ]

                    angle =
                        Vector.angleBetween a b
                in
                angle
                    |> Expect.within (Expect.Absolute 0.000000001) Basics.pi
        ]
