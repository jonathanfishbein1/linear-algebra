module VectorTests.VectorTests exposing (suite)

import CommutativeDivisionRing
import ComplexNumbers
import Expect
import Field
import Float.Extra
import Fuzz
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Vector module"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests hadamard vector multiplication is commutative"
          <|
            \one two ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    aHadamardB =
                        Vector.hadamardMultiplication
                            Field.float
                            a
                            b

                    bhadamardA =
                        Vector.hadamardMultiplication
                            Field.float
                            b
                            a
                in
                Expect.true "vectors equal" (Vector.equal (==) aHadamardB bhadamardA)
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests hadamard vector multiplication is associative"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    c =
                        Vector.Vector [ three ]

                    aHadamardBHadamardC =
                        Vector.hadamardMultiplication
                            Field.float
                            (Vector.hadamardMultiplication Field.float a b)
                            c

                    bHadamardCHadamardA =
                        Vector.hadamardMultiplication
                            Field.float
                            a
                            (Vector.hadamardMultiplication Field.float b c)
                in
                Expect.true "vectors equal" (Vector.equal (Float.Extra.equalWithin 0.1) aHadamardBHadamardC bHadamardCHadamardA)
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests hadamard vector multiplication is distributive over addition"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector [ one ]

                    b =
                        Vector.Vector [ two ]

                    c =
                        Vector.Vector [ three ]

                    aHadamardSumBC =
                        Vector.hadamardMultiplication
                            Field.float
                            a
                            (Vector.add Field.float b c)

                    sumAHadamardBAHadamardC =
                        Vector.add
                            Field.float
                            (Vector.hadamardMultiplication Field.float a b)
                            (Vector.hadamardMultiplication Field.float a c)
                in
                Expect.true "vectors equal" (Vector.equal (Float.Extra.equalWithin 0.1) aHadamardSumBC sumAHadamardBAHadamardC)
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests cross product is orthagonal to both vectors"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector3 one two three

                    b =
                        Vector.Vector3 two three one

                    (Field.Field commutativeDivisionRing) =
                        Field.float

                    aCrossB =
                        Vector.cross commutativeDivisionRing a b
                            |> Vector.vector3ToVector

                    aDotACrossB =
                        Vector.dotProduct Field.float (Vector.vector3ToVector a) aCrossB

                    bDotACrossB =
                        Vector.dotProduct Field.float (Vector.vector3ToVector b) aCrossB

                    result =
                        Float.Extra.equalWithin 0.000000001 0 aDotACrossB && Float.Extra.equalWithin 0.000000001 0 bDotACrossB
                in
                Expect.true "a X b is orthagonal to both a and b" result
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests length of cross product is the length of the two vectors times the sin of the angle between them"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector3 one two three

                    b =
                        Vector.Vector3 two three one

                    aCrossB =
                        Vector.cross CommutativeDivisionRing.float a b
                            |> Vector.vector3ToVector

                    aVector =
                        Vector.vector3ToVector a

                    bVector =
                        Vector.vector3ToVector b

                    aLength =
                        Vector.lengthReal aVector

                    bLength =
                        Vector.lengthReal bVector

                    aCrossBLength =
                        Vector.lengthReal aCrossB

                    angle =
                        Vector.angleBetween aVector bVector
                in
                Expect.within (Expect.Absolute 0.0001) aCrossBLength (aLength * bLength * Basics.sin angle)
        , Test.fuzz
            (Fuzz.floatRange 1 10)
            "tests unit vector length is 1"
          <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    normalisedALength =
                        Vector.normaliseReal a
                            |> Vector.lengthReal
                in
                Expect.within (Expect.Absolute 0.000000001)
                    normalisedALength
                    1
        , Test.fuzz
            (Fuzz.floatRange 1 10)
            "tests complex unit vector length is 1"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                one
                            )

                    a =
                        Vector.Vector [ complexNumber ]

                    normalisedALength =
                        Vector.normaliseComplex a
                            |> Vector.lengthComplex
                in
                Expect.within (Expect.Absolute 0.000000001)
                    normalisedALength
                    1
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests realVectorSubspace"
          <|
            \one two ->
                let
                    vectors =
                        [ Vector.Vector
                            [ one
                            ]
                        , Vector.Vector
                            [ two
                            ]
                        ]

                    predicates =
                        [ \_ -> True ]

                    scalar =
                        Vector.Scalar one

                    isSubspace =
                        Vector.vectorSubspace Field.float Vector.realVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.true "is a subspace"
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests realVectorSubspace x > 10 not a subspace"
          <|
            \one two ->
                let
                    vectors =
                        [ Vector.Vector
                            [ one
                            ]
                        , Vector.Vector
                            [ two
                            ]
                        ]

                    predicates =
                        [ (>) 0 ]

                    scalar =
                        Vector.Scalar one

                    isSubspace =
                        Vector.vectorSubspace Field.float Vector.realVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.false "is not a subspace"
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests complexVectorSubspace"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    vectors =
                        [ Vector.Vector
                            [ complexNumber
                            ]
                        ]

                    predicates =
                        [ \_ -> True ]

                    scalar =
                        Vector.Scalar complexNumber

                    isSubspace =
                        Vector.vectorSubspace ComplexNumbers.complexField Vector.complexVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.true "is a subspace"
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests complexVectorSubspace x > zero not a subspace"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    vectors =
                        [ Vector.Vector
                            [ complexNumber
                            ]
                        ]

                    complexOne =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                1
                            )
                            (ComplexNumbers.Imaginary
                                0
                            )

                    predicates =
                        [ ComplexNumbers.equal complexOne ]

                    scalar =
                        Vector.Scalar complexNumber

                    isSubspace =
                        Vector.vectorSubspace ComplexNumbers.complexField Vector.complexVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.false "is not a subspace"
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "getAt index"
          <|
            \one two three ->
                let
                    vector =
                        Vector.Vector [ one, two, three ]
                in
                Expect.equal (Vector.getAt 0 vector) (Just one)
        , Test.fuzz
            Fuzz.int
            "setAt getAt index"
          <|
            \one ->
                let
                    vector =
                        Vector.setAt 0 one (Vector.Vector [ 0 ])
                in
                Expect.equal (Vector.getAt 0 vector) (Just one)
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "read Vector"
          <|
            \one two ->
                let
                    vector =
                        Vector.Vector [ one, two ]

                    printedVector =
                        Vector.printRealVector vector

                    readVector =
                        Vector.readRealVector printedVector
                in
                Expect.equal readVector (Ok vector)
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests subtractRealVectors"
          <|
            \one two ->
                let
                    vectorOne =
                        Vector.Vector [ one, two ]

                    vectorTwo =
                        Vector.Vector [ one, two ]

                    result =
                        Vector.subtract Field.float vectorOne vectorTwo
                in
                Expect.equal result (Vector.Vector [ 0, 0 ])
        , Test.test
            "tests vector tensor product"
          <|
            \_ ->
                let
                    vectorOne =
                        Vector.Vector [ 1, 2 ]

                    vectorTwo =
                        Vector.Vector [ 3, 4 ]

                    tensorProduct =
                        Vector.tensorProduct Field.float vectorOne vectorTwo
                in
                Expect.equal tensorProduct (Vector.Vector [ 3, 4, 6, 8 ])
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests vector tensor product respects addition"
          <|
            \one two three ->
                let
                    vectorI =
                        Vector.Vector [ one, two ]

                    vectorJ =
                        Vector.Vector [ three, one ]

                    vectorK =
                        Vector.Vector [ two, three ]

                    vectorSumIJ =
                        Vector.add Field.float vectorI vectorJ

                    tensorProductIJK =
                        Vector.tensorProduct Field.float vectorSumIJ vectorK

                    tensorProductIK =
                        Vector.tensorProduct Field.float vectorI vectorK

                    tensorProductJK =
                        Vector.tensorProduct Field.float vectorJ vectorK

                    vectorSumTensorProductIKJK =
                        Vector.add Field.float tensorProductIK tensorProductJK
                in
                Expect.true "vectors equal" (Vector.equal (\valOne valTwo -> Float.Extra.equalWithin 0.1 valOne valTwo) tensorProductIJK vectorSumTensorProductIKJK)
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests vector tensor product respects scalar multiplication"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real
                                three
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    vectorJ =
                        Vector.Vector [ complexNumberTwo, complexNumberThree ]

                    vectorK =
                        Vector.Vector [ complexNumberThree, complexNumberTwo ]

                    tensorProductJK =
                        Vector.tensorProduct ComplexNumbers.complexField vectorJ vectorK

                    cScalarMultiplicationtensorProductJK =
                        Vector.scalarMultiplication ComplexNumbers.complexField complexNumberOne tensorProductJK

                    cScalarMultiplicationVectorJ =
                        Vector.scalarMultiplication ComplexNumbers.complexField complexNumberOne vectorJ

                    cScalarMultiplicationVectorJTensorProductVectorK =
                        Vector.tensorProduct ComplexNumbers.complexField cScalarMultiplicationVectorJ vectorK
                in
                Expect.true "vectors equal" (Vector.equal ComplexNumbers.equal cScalarMultiplicationtensorProductJK cScalarMultiplicationVectorJTensorProductVectorK)
        ]
