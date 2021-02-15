module VectorTests.VectorTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Float.Extra
import Fuzz
import Imaginary
import Real
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Vector module"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests hadamard vector multiplication is commutative"
          <|
            \one two ->
                let
                    a =
                        Vector.pure one

                    b =
                        Vector.pure two

                    aHadamardB =
                        Vector.hadamardMultiplication
                            Real.field
                            a
                            b

                    bhadamardA =
                        Vector.hadamardMultiplication
                            Real.field
                            b
                            a
                in
                Expect.true "hadamard vector multiplication is commutative" ((Vector.equal Real.equal.eq).eq aHadamardB bhadamardA)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests hadamard vector multiplication is associative"
          <|
            \one two three ->
                let
                    a =
                        Vector.pure one

                    b =
                        Vector.pure two

                    c =
                        Vector.pure three

                    aHadamardBHadamardC =
                        Vector.hadamardMultiplication
                            Real.field
                            (Vector.hadamardMultiplication Real.field a b)
                            c

                    bHadamardCHadamardA =
                        Vector.hadamardMultiplication
                            Real.field
                            a
                            (Vector.hadamardMultiplication Real.field b c)
                in
                Expect.true "hadamard vector multiplication is associative" ((Vector.equal Real.equal.eq).eq aHadamardBHadamardC bHadamardCHadamardA)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests hadamard vector multiplication is distributive over addition"
          <|
            \one two three ->
                let
                    a =
                        Vector.pure one

                    b =
                        Vector.pure two

                    c =
                        Vector.pure three

                    aHadamardSumBC =
                        Vector.hadamardMultiplication
                            Real.field
                            a
                            (Vector.add Real.field b c)

                    sumAHadamardBAHadamardC =
                        Vector.add
                            Real.field
                            (Vector.hadamardMultiplication Real.field a b)
                            (Vector.hadamardMultiplication Real.field a c)
                in
                Expect.true "hadamard vector multiplication is distributative over addition" ((Vector.equal Real.equal.eq).eq aHadamardSumBC sumAHadamardBAHadamardC)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests cross product is orthagonal to both vectors"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector3 one two three

                    b =
                        Vector.Vector3 two three one

                    (Field.Field commutativeDivisionRing) =
                        Real.field

                    aCrossB =
                        Vector.cross commutativeDivisionRing a b
                            |> Vector.vector3ToVector

                    aDotACrossB =
                        Vector.dotProduct Real.field (Vector.vector3ToVector a) aCrossB
                            |> Real.real

                    bDotACrossB =
                        Vector.dotProduct Real.field (Vector.vector3ToVector b) aCrossB
                            |> Real.real

                    result =
                        Float.Extra.equalWithin 0.000000001 0 aDotACrossB && Float.Extra.equalWithin 0.000000001 0 bDotACrossB
                in
                Expect.true "a X b is orthagonal to both a and b" result
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests length of cross product is the length of the two vectors times the sin of the angle between them"
          <|
            \one two three ->
                let
                    a =
                        Vector.Vector3 one two three

                    b =
                        Vector.Vector3 two three one

                    aCrossB =
                        Vector.cross Real.commutativeDivisionRing a b
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
                Expect.true "length of cross product is the length of the two vectors times the sin of the angle between them" (Real.equal.eq aCrossBLength (Real.multiply (Real.multiply aLength bLength) (Real.map Basics.sin angle)))
        , Test.fuzz
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            "tests unit vector length is 1"
          <|
            \one ->
                let
                    a =
                        Vector.pure one

                    normalisedALength =
                        Vector.normaliseReal a
                            |> Vector.lengthReal
                in
                Expect.true "unit vector length is 1" (Real.equal.eq normalisedALength Real.one)
        , Test.fuzz
            (Fuzz.floatRange 1 10)
            "tests complex unit vector length is 1"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                one
                            )
                            (Imaginary.Imaginary
                                one
                            )

                    a =
                        Vector.pure complexNumber

                    normalisedALength =
                        Vector.normaliseComplex a
                            |> Vector.lengthComplex
                in
                Expect.true "complex unit vector length is 1"
                    (Real.equal.eq normalisedALength Real.one)
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests realVectorSubspace"
          <|
            \one two ->
                let
                    vectors =
                        [ Vector.pure
                            one
                        , Vector.pure
                            two
                        ]

                    predicates =
                        [ \_ -> True ]

                    scalar =
                        Vector.Scalar one

                    isSubspace =
                        Vector.vectorSubspace Real.field Vector.realVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.true "is a subspace"
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests realVectorSubspace x > 10 not a subspace"
          <|
            \one two ->
                let
                    vectors =
                        [ Vector.pure one
                        , Vector.pure two
                        ]

                    predicates =
                        [ \(Real.Real real) -> real > 0 ]

                    scalar =
                        Vector.Scalar one

                    isSubspace =
                        Vector.vectorSubspace Real.field Vector.realVectorAbelianGroup scalar vectors predicates
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
                            (Real.Real
                                one
                            )
                            (Imaginary.Imaginary
                                two
                            )

                    vectors =
                        [ Vector.pure
                            complexNumber
                        ]

                    predicates =
                        [ \_ -> True ]

                    scalar =
                        Vector.Scalar complexNumber

                    isSubspace =
                        Vector.vectorSubspace ComplexNumbers.field Vector.complexVectorAbelianGroup scalar vectors predicates
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
                            (Real.Real
                                one
                            )
                            (Imaginary.Imaginary
                                two
                            )

                    vectors =
                        [ Vector.pure complexNumber
                        ]

                    complexOne =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                1
                            )
                            (Imaginary.Imaginary
                                0
                            )

                    predicates =
                        [ ComplexNumbers.equal.eq complexOne ]

                    scalar =
                        Vector.Scalar complexNumber

                    isSubspace =
                        Vector.vectorSubspace ComplexNumbers.field Vector.complexVectorAbelianGroup scalar vectors predicates
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
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests subtractRealVectors"
          <|
            \one two ->
                let
                    vectorOne =
                        Vector.Vector [ one, two ]

                    vectorTwo =
                        Vector.Vector [ one, two ]

                    result =
                        Vector.subtract Real.field vectorOne vectorTwo
                in
                Expect.equal result (Vector.Vector [ Real.zero, Real.zero ])
        , Test.test
            "tests vector tensor product"
          <|
            \_ ->
                let
                    vectorOne =
                        Vector.Vector [ Real.one, Real.Real 2 ]

                    vectorTwo =
                        Vector.Vector [ Real.Real 3, Real.Real 4 ]

                    tensorProduct =
                        Vector.tensorProduct Real.field vectorOne vectorTwo
                in
                Expect.equal tensorProduct (Vector.Vector [ Real.Real 3, Real.Real 4, Real.Real 6, Real.Real 8 ])
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
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
                        Vector.add Real.field vectorI vectorJ

                    tensorProductIJK =
                        Vector.tensorProduct Real.field vectorSumIJ vectorK

                    tensorProductIK =
                        Vector.tensorProduct Real.field vectorI vectorK

                    tensorProductJK =
                        Vector.tensorProduct Real.field vectorJ vectorK

                    vectorSumTensorProductIKJK =
                        Vector.add Real.field tensorProductIK tensorProductJK
                in
                Expect.true "tensor product respects addition" ((Vector.equal Real.equal.eq).eq tensorProductIJK vectorSumTensorProductIKJK)
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
                            (Real.Real
                                one
                            )
                            (Imaginary.Imaginary
                                two
                            )

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                one
                            )
                            (Imaginary.Imaginary
                                two
                            )

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                three
                            )
                            (Imaginary.Imaginary
                                two
                            )

                    vectorJ =
                        Vector.Vector [ complexNumberTwo, complexNumberThree ]

                    vectorK =
                        Vector.Vector [ complexNumberThree, complexNumberTwo ]

                    tensorProductJK =
                        Vector.tensorProduct ComplexNumbers.field vectorJ vectorK

                    cScalarMultiplicationtensorProductJK =
                        Vector.scalarMultiplication ComplexNumbers.field complexNumberOne tensorProductJK

                    cScalarMultiplicationVectorJ =
                        Vector.scalarMultiplication ComplexNumbers.field complexNumberOne vectorJ

                    cScalarMultiplicationVectorJTensorProductVectorK =
                        Vector.tensorProduct ComplexNumbers.field cScalarMultiplicationVectorJ vectorK
                in
                Expect.true "vectors equal" ((Vector.equal ComplexNumbers.equal.eq).eq cScalarMultiplicationtensorProductJK cScalarMultiplicationVectorJTensorProductVectorK)
        ]
