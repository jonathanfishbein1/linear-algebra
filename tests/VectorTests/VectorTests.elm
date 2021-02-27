module VectorTests.VectorTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Vector
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
                        Internal.Vector.pure one

                    b =
                        Internal.Vector.pure two

                    aHadamardB =
                        Internal.Vector.hadamardMultiplication
                            Real.field
                            a
                            b

                    bhadamardA =
                        Internal.Vector.hadamardMultiplication
                            Real.field
                            b
                            a
                in
                Expect.true "hadamard vector multiplication is commutative" ((Internal.Vector.equal Real.equal.eq).eq aHadamardB bhadamardA)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests hadamard vector multiplication is associative"
          <|
            \one two three ->
                let
                    a =
                        Internal.Vector.pure one

                    b =
                        Internal.Vector.pure two

                    c =
                        Internal.Vector.pure three

                    aHadamardBHadamardC =
                        Internal.Vector.hadamardMultiplication
                            Real.field
                            (Internal.Vector.hadamardMultiplication Real.field a b)
                            c

                    bHadamardCHadamardA =
                        Internal.Vector.hadamardMultiplication
                            Real.field
                            a
                            (Internal.Vector.hadamardMultiplication Real.field b c)
                in
                Expect.true "hadamard vector multiplication is associative" ((Internal.Vector.equal Real.equal.eq).eq aHadamardBHadamardC bHadamardCHadamardA)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests hadamard vector multiplication is distributive over addition"
          <|
            \one two three ->
                let
                    a =
                        Internal.Vector.pure one

                    b =
                        Internal.Vector.pure two

                    c =
                        Internal.Vector.pure three

                    aHadamardSumBC =
                        Internal.Vector.hadamardMultiplication
                            Real.field
                            a
                            (Internal.Vector.add Real.field b c)

                    sumAHadamardBAHadamardC =
                        Internal.Vector.add
                            Real.field
                            (Internal.Vector.hadamardMultiplication Real.field a b)
                            (Internal.Vector.hadamardMultiplication Real.field a c)
                in
                Expect.true "hadamard vector multiplication is distributative over addition" ((Internal.Vector.equal Real.equal.eq).eq aHadamardSumBC sumAHadamardBAHadamardC)
        , Test.fuzz
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            "tests unit vector length is 1"
          <|
            \one ->
                let
                    a =
                        Internal.Vector.pure one

                    normalisedALength =
                        Internal.Vector.normaliseReal a
                            |> Internal.Vector.lengthReal
                in
                Expect.true "unit vector length is 1" (Real.equal.eq normalisedALength Real.one)
        , Test.fuzz
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            "tests complex unit vector length is 1"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                one
                            )

                    a =
                        Internal.Vector.pure complexNumber

                    normalisedALength =
                        Internal.Vector.normaliseComplex a
                            |> Internal.Vector.lengthComplex
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
                        [ Internal.Vector.pure
                            one
                        , Internal.Vector.pure
                            two
                        ]

                    predicates =
                        [ \_ -> True ]

                    scalar =
                        Internal.Vector.Scalar one

                    isSubspace =
                        Internal.Vector.vectorSubspace Real.field Internal.Vector.realAbelianGroup scalar vectors predicates
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
                        [ Internal.Vector.pure one
                        , Internal.Vector.pure two
                        ]

                    predicates =
                        [ \(Real.Real real) -> real > 0 ]

                    scalar =
                        Internal.Vector.Scalar one

                    isSubspace =
                        Internal.Vector.vectorSubspace Real.field Internal.Vector.realAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.false "is not a subspace"
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests complexVectorSubspace"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    vectors =
                        [ Internal.Vector.pure
                            complexNumber
                        ]

                    predicates =
                        [ \_ -> True ]

                    scalar =
                        Internal.Vector.Scalar complexNumber

                    isSubspace =
                        Internal.Vector.vectorSubspace ComplexNumbers.field Internal.Vector.complexAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.true "is a subspace"
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests complexVectorSubspace x > zero not a subspace"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    vectors =
                        [ Internal.Vector.pure complexNumber
                        ]

                    complexOne =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            Imaginary.zero

                    predicates =
                        [ ComplexNumbers.equal.eq complexOne ]

                    scalar =
                        Internal.Vector.Scalar complexNumber

                    isSubspace =
                        Internal.Vector.vectorSubspace ComplexNumbers.field Internal.Vector.complexAbelianGroup scalar vectors predicates
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
                Expect.equal (Internal.Vector.getAt 0 vector) (Just one)
        , Test.fuzz
            Fuzz.int
            "setAt getAt index"
          <|
            \one ->
                let
                    vector =
                        Internal.Vector.setAt 0 one (Vector.Vector [ 0 ])
                in
                Expect.equal (Internal.Vector.getAt 0 vector) (Just one)
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "read Vector"
          <|
            \one two ->
                let
                    vector =
                        Vector.Vector [ one, two ]

                    printedVector =
                        Internal.Vector.printRealVector vector

                    readVector =
                        Internal.Vector.readRealVector printedVector
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
                        Internal.Vector.subtract Real.field vectorOne vectorTwo
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
                        Internal.Vector.tensorProduct Real.field vectorOne vectorTwo
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
                        Internal.Vector.add Real.field vectorI vectorJ

                    tensorProductIJK =
                        Internal.Vector.tensorProduct Real.field vectorSumIJ vectorK

                    tensorProductIK =
                        Internal.Vector.tensorProduct Real.field vectorI vectorK

                    tensorProductJK =
                        Internal.Vector.tensorProduct Real.field vectorJ vectorK

                    vectorSumTensorProductIKJK =
                        Internal.Vector.add Real.field tensorProductIK tensorProductJK
                in
                Expect.true "tensor product respects addition" ((Internal.Vector.equal Real.equal.eq).eq tensorProductIJK vectorSumTensorProductIKJK)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests vector tensor product respects scalar multiplication"
          <|
            \one two three ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary
                                two
                            )

                    complexNumberThree =
                        ComplexNumbers.ComplexNumber
                            three
                            (Imaginary.Imaginary
                                two
                            )

                    vectorJ =
                        Vector.Vector [ complexNumberTwo, complexNumberThree ]

                    vectorK =
                        Vector.Vector [ complexNumberThree, complexNumberTwo ]

                    tensorProductJK =
                        Internal.Vector.tensorProduct ComplexNumbers.field vectorJ vectorK

                    cScalarMultiplicationtensorProductJK =
                        Internal.Vector.scalarMultiplication ComplexNumbers.field complexNumberOne tensorProductJK

                    cScalarMultiplicationVectorJ =
                        Internal.Vector.scalarMultiplication ComplexNumbers.field complexNumberOne vectorJ

                    cScalarMultiplicationVectorJTensorProductVectorK =
                        Internal.Vector.tensorProduct ComplexNumbers.field cScalarMultiplicationVectorJ vectorK
                in
                Expect.true "vectors equal" ((Internal.Vector.equal ComplexNumbers.equal.eq).eq cScalarMultiplicationtensorProductJK cScalarMultiplicationVectorJTensorProductVectorK)
        ]
