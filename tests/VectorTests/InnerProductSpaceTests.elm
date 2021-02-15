module VectorTests.InnerProductSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "Inner Product Vector Space"
        [ Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "tests dot product is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.dotProduct Real.field a a
                            |> Real.real
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
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
                            Real.field
                            (Vector.add Real.field a b)
                            c

                    aDotC =
                        Vector.dotProduct Real.field a c

                    bDotC =
                        Vector.dotProduct Real.field b c

                    aDotCPlusBDotC =
                        Real.add aDotC bDotC
                in
                Expect.true "dot product respects addition" (Real.equal.eq aDotCPlusBDotC aPlusBDotc)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
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
                            Real.field
                            (Vector.scalarMultiplication Real.field three a)
                            b

                    aDotBTimesThree =
                        Real.multiply (Vector.dotProduct Real.field a b) three
                in
                Expect.true "dot product respects scalar multiplication" (Real.equal.eq aDotBTimesThree threeTimesADotB)
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests dot product is symetric"
          <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    aDotB =
                        Vector.dotProduct Real.field a b

                    bDotA =
                        Vector.dotProduct Real.field b a
                in
                Expect.equal bDotA aDotB
        , Test.fuzz
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests vector length equals square root of dot product"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    squareRootADotA =
                        Real.map Basics.sqrt (Vector.dotProduct Real.field a a)

                    aLength =
                        Vector.lengthReal a
                in
                Expect.equal squareRootADotA aLength
        , Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "tests vector length is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.lengthReal a
                            |> Real.real
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
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
                            Real.field
                            x
                            y
                            |> Real.map Basics.abs
                            |> Real.real

                    lengthOfX =
                        Vector.lengthReal x

                    lengthOfY =
                        Vector.lengthReal y

                    lengthOfXTimesLengthOfY =
                        Real.multiply lengthOfX lengthOfY
                            |> Real.real
                in
                absXDotY
                    |> Expect.atMost lengthOfXTimesLengthOfY
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
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
                            Real.field
                            a
                            b
                            |> Vector.lengthReal
                            |> Real.real

                    lengthAPlusLengthB =
                        Real.add
                            (Vector.lengthReal a)
                            (Vector.lengthReal b)
                            |> Real.real
                in
                aPlusBLength
                    |> Expect.atMost lengthAPlusLengthB
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests vector length respects scalar multiplication"
          <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    legnthOfTwoTimesA =
                        Vector.lengthReal
                            (Vector.scalarMultiplication Real.field two a)

                    lengthOfATimesTwo =
                        Vector.lengthReal a
                            |> Real.multiply two
                            |> Real.map Basics.abs
                in
                Expect.equal legnthOfTwoTimesA lengthOfATimesTwo
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
                            |> Real.map Basics.sqrt

                    aLength =
                        Vector.lengthComplex a
                in
                Expect.equal squareRootADotA aLength
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
                            |> Real.real
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
                            |> Real.map Basics.abs
                            |> Real.real

                    lengthOfX =
                        Vector.lengthComplex x

                    lengthOfY =
                        Vector.lengthComplex y

                    lengthOfXTimesLengthOfY =
                        Real.multiply lengthOfX lengthOfY
                            |> Real.real
                in
                absXDotY
                    |> Expect.atMost lengthOfXTimesLengthOfY
        , Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "tests distance is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    expected =
                        Vector.distanceReal a a
                            |> Real.real
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz3
            (Fuzz.map (toFloat >> Real.Real) Fuzz.int)
            (Fuzz.map (toFloat >> Real.Real) Fuzz.int)
            (Fuzz.map (toFloat >> Real.Real) Fuzz.int)
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
                            |> Real.real

                    distanceAC =
                        Vector.distanceReal a c
                            |> Real.real

                    distanceCB =
                        Vector.distanceReal c b
                            |> Real.real
                in
                distanceAB
                    |> Expect.atMost (distanceAC + distanceCB)
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
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
                            Real.field
                            a
                            b

                    distanceBA =
                        Vector.dotProduct
                            Real.field
                            b
                            a
                in
                Expect.equal distanceAB distanceBA
        , Test.test
            "tests angle between orthagonal vectors"
          <|
            \_ ->
                let
                    a =
                        Vector.Vector [ Real.one, Real.zero ]

                    b =
                        Vector.Vector [ Real.zero, Real.one ]

                    angle =
                        Vector.angleBetween a b
                in
                Expect.equal angle (Real.Real (Basics.pi / 2))
        , Test.test
            "tests angle between colinear vectors"
          <|
            \_ ->
                let
                    a =
                        Vector.Vector [ Real.one, Real.zero ]

                    b =
                        Vector.Vector [ Real.one, Real.zero ]

                    angle =
                        Vector.angleBetween a b
                in
                Expect.equal angle Real.zero
        , Test.test
            "tests angle between colinear but opposite vectors"
          <|
            \_ ->
                let
                    a =
                        Vector.Vector [ Real.one, Real.zero ]

                    b =
                        Vector.Vector [ Real.Real -1, Real.zero ]

                    angle =
                        Vector.angleBetween a b
                in
                Expect.equal angle (Real.Real Basics.pi)
        ]
