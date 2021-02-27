module VectorTests.InnerProductSpaceTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
import Real
import Test


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
                        Internal.Vector.Vector [ one ]

                    expected =
                        Internal.Vector.dotProduct Real.field a a
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
                        Internal.Vector.Vector [ one ]

                    b =
                        Internal.Vector.Vector [ two ]

                    c =
                        Internal.Vector.Vector [ three ]

                    aPlusBDotc =
                        Internal.Vector.dotProduct
                            Real.field
                            (Internal.Vector.add Real.field a b)
                            c

                    aDotC =
                        Internal.Vector.dotProduct Real.field a c

                    bDotC =
                        Internal.Vector.dotProduct Real.field b c

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
                        Internal.Vector.Vector [ one ]

                    b =
                        Internal.Vector.Vector [ two ]

                    threeTimesADotB =
                        Internal.Vector.dotProduct
                            Real.field
                            (Internal.Vector.scalarMultiplication Real.field three a)
                            b

                    aDotBTimesThree =
                        Real.multiply (Internal.Vector.dotProduct Real.field a b) three
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
                        Internal.Vector.Vector [ one ]

                    b =
                        Internal.Vector.Vector [ two ]

                    aDotB =
                        Internal.Vector.dotProduct Real.field a b

                    bDotA =
                        Internal.Vector.dotProduct Real.field b a
                in
                Expect.equal bDotA aDotB
        , Test.fuzz
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests vector length equals square root of dot product"
          <|
            \one ->
                let
                    a =
                        Internal.Vector.Vector [ one ]

                    squareRootADotA =
                        Real.map Basics.sqrt (Internal.Vector.dotProduct Real.field a a)

                    aLength =
                        Internal.Vector.lengthReal a
                in
                Expect.equal squareRootADotA aLength
        , Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "tests vector length is nondegenerative"
          <|
            \one ->
                let
                    a =
                        Internal.Vector.Vector [ one ]

                    expected =
                        Internal.Vector.lengthReal a
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
                        Internal.Vector.Vector [ one ]

                    y =
                        Internal.Vector.Vector [ two ]

                    absXDotY =
                        Internal.Vector.dotProduct
                            Real.field
                            x
                            y
                            |> Real.map Basics.abs
                            |> Real.real

                    lengthOfX =
                        Internal.Vector.lengthReal x

                    lengthOfY =
                        Internal.Vector.lengthReal y

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
                        Internal.Vector.Vector [ one ]

                    b =
                        Internal.Vector.Vector [ two ]

                    aPlusBLength =
                        Internal.Vector.add
                            Real.field
                            a
                            b
                            |> Internal.Vector.lengthReal
                            |> Real.real

                    lengthAPlusLengthB =
                        Real.add
                            (Internal.Vector.lengthReal a)
                            (Internal.Vector.lengthReal b)
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
                        Internal.Vector.Vector [ one ]

                    legnthOfTwoTimesA =
                        Internal.Vector.lengthReal
                            (Internal.Vector.scalarMultiplication Real.field two a)

                    lengthOfATimesTwo =
                        Internal.Vector.lengthReal a
                            |> Real.multiply two
                            |> Real.map Basics.abs
                in
                Expect.equal legnthOfTwoTimesA lengthOfATimesTwo
        , Test.fuzz
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests complex vector length equals square root of dot product"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber one (Imaginary.Imaginary one)

                    a =
                        Internal.Vector.Vector [ complexNumber ]

                    squareRootADotA =
                        Internal.Vector.dotProduct ComplexNumbers.field a (Internal.Vector.conjugate a)
                            |> ComplexNumbers.real
                            |> Real.map Basics.sqrt

                    aLength =
                        Internal.Vector.lengthComplex a
                in
                Expect.equal squareRootADotA aLength
        , Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "tests complex vector length is nondegenerative"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber one (Imaginary.Imaginary one)

                    a =
                        Internal.Vector.Vector [ complexNumber ]

                    expected =
                        Internal.Vector.lengthComplex a
                            |> Real.real
                in
                expected
                    |> Expect.atLeast 0
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests complex vector dot product satisfies Cauchy-Shwartz inequality"
          <|
            \one two ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber one (Imaginary.Imaginary one)

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber two (Imaginary.Imaginary two)

                    x =
                        Internal.Vector.Vector [ complexNumberOne ]

                    y =
                        Internal.Vector.Vector [ complexNumberTwo ]

                    absXDotY =
                        Internal.Vector.dotProduct
                            ComplexNumbers.field
                            x
                            y
                            |> ComplexNumbers.real
                            |> Real.map Basics.abs
                            |> Real.real

                    lengthOfX =
                        Internal.Vector.lengthComplex x

                    lengthOfY =
                        Internal.Vector.lengthComplex y

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
                        Internal.Vector.Vector [ one ]

                    expected =
                        Internal.Vector.distanceReal a a
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
                        Internal.Vector.Vector [ one ]

                    b =
                        Internal.Vector.Vector [ two ]

                    c =
                        Internal.Vector.Vector [ three ]

                    distanceAB =
                        Internal.Vector.distanceReal a b
                            |> Real.real

                    distanceAC =
                        Internal.Vector.distanceReal a c
                            |> Real.real

                    distanceCB =
                        Internal.Vector.distanceReal c b
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
                        Internal.Vector.Vector [ one ]

                    b =
                        Internal.Vector.Vector [ two ]

                    distanceAB =
                        Internal.Vector.dotProduct
                            Real.field
                            a
                            b

                    distanceBA =
                        Internal.Vector.dotProduct
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
                        Internal.Vector.Vector [ Real.one, Real.zero ]

                    b =
                        Internal.Vector.Vector [ Real.zero, Real.one ]

                    angle =
                        Internal.Vector.angleBetween a b
                in
                Expect.equal angle (Real.Real (Basics.pi / 2))
        , Test.test
            "tests angle between colinear vectors"
          <|
            \_ ->
                let
                    a =
                        Internal.Vector.Vector [ Real.one, Real.zero ]

                    b =
                        Internal.Vector.Vector [ Real.one, Real.zero ]

                    angle =
                        Internal.Vector.angleBetween a b
                in
                Expect.equal angle Real.zero
        , Test.test
            "tests angle between colinear but opposite vectors"
          <|
            \_ ->
                let
                    a =
                        Internal.Vector.Vector [ Real.one, Real.zero ]

                    b =
                        Internal.Vector.Vector [ Real.Real -1, Real.zero ]

                    angle =
                        Internal.Vector.angleBetween a b
                in
                Expect.equal angle (Real.Real Basics.pi)
        ]
