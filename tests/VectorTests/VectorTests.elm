module VectorTests.VectorTests exposing (suite)

import ComplexNumbers
import Expect
import Field
import Float.Extra
import Fuzz
import Parser
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The Vector module"
        [ Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests cross product is orthagonal to both vectors" <|
            \one two three ->
                let
                    a =
                        Vector.Vector3 one two three

                    b =
                        Vector.Vector3 two three one

                    aCrossB =
                        Vector.cross Field.realField a b
                            |> Vector.vector3ToVector

                    aDotACrossB =
                        Vector.vectorDotProduct Field.realField (Vector.vector3ToVector a) aCrossB
                in
                aDotACrossB
                    |> Expect.equal 0
        , Test.fuzz (Fuzz.map toFloat (Fuzz.intRange 1 10)) "tests unit vector length is 1" <|
            \one ->
                let
                    a =
                        Vector.Vector [ one ]

                    normalisedALength =
                        Vector.normalise a
                            |> Vector.vectorLength Field.realField
                in
                normalisedALength
                    |> Expect.equal 1
        , Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests realVectorSubspace" <|
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
                        Vector.vectorSubspace Vector.realVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.true "is a subspace"
        , Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests realVectorSubspace x > 10 not a subspace" <|
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
                        Vector.vectorSubspace Vector.realVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.false "is not a subspace"
        , Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests complexVectorSubspace" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian
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
                        Vector.vectorSubspace Vector.complexVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.true "is a subspace"
        , Test.fuzz2 (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int) "tests complexVectorSubspace x > zero not a subspace" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian
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
                        ComplexNumbers.ComplexNumberCartesian
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
                        Vector.vectorSubspace Vector.complexVectorAbelianGroup scalar vectors predicates
                in
                isSubspace
                    |> Expect.false "is not a subspace"
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "getAt index" <|
            \one two three ->
                let
                    vector =
                        Vector.Vector [ one, two, three ]
                in
                Expect.equal (Vector.getAt 0 vector) (Just one)
        , Test.fuzz Fuzz.int "setAt getAt index" <|
            \one ->
                let
                    vector =
                        Vector.setAt 0 one (Vector.Vector [ 0 ])
                in
                Expect.equal (Vector.getAt 0 vector) (Just one)
        , Test.fuzz2 Fuzz.float Fuzz.float "read Vector" <|
            \one two ->
                let
                    vector =
                        Vector.Vector [ one, two ]

                    printedVector =
                        Vector.print vector

                    readVector =
                        Vector.read printedVector
                in
                Expect.equal readVector (Ok vector)
        , Test.fuzz2 Fuzz.float Fuzz.float "tests subtractRealVectors" <|
            \one two ->
                let
                    vectorOne =
                        Vector.Vector [ one, two ]

                    vectorTwo =
                        Vector.Vector [ one, two ]

                    result =
                        Vector.subtractVectors Field.realField vectorOne vectorTwo
                in
                Expect.equal result (Vector.Vector [ 0, 0 ])
        , Test.fuzz2 Fuzz.float Fuzz.float "tests subtractComplexVectors" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    vectorOne =
                        Vector.Vector [ complexNumber ]

                    vectorTwo =
                        Vector.Vector [ complexNumber ]

                    result =
                        Vector.subtractVectors ComplexNumbers.complexField vectorOne vectorTwo
                in
                Expect.equal result (Vector.Vector [ ComplexNumbers.zero ])
        , Test.test "tests vector tensor product" <|
            \_ ->
                let
                    vectorOne =
                        Vector.Vector [ 1, 2 ]

                    vectorTwo =
                        Vector.Vector [ 3, 4 ]

                    vectorTensorProduct =
                        Vector.vectorTensorProduct Field.realField vectorOne vectorTwo
                in
                Expect.equal vectorTensorProduct (Vector.Vector [ 3, 4, 6, 8 ])
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests vector tensor product respects addition" <|
            \one two three ->
                let
                    vectorI =
                        Vector.Vector [ one, two ]

                    vectorJ =
                        Vector.Vector [ three, one ]

                    vectorK =
                        Vector.Vector [ two, three ]

                    vectorSumIJ =
                        Vector.addVectors Field.realField vectorI vectorJ

                    vectorTensorProductIJK =
                        Vector.vectorTensorProduct Field.realField vectorSumIJ vectorK

                    vectorTensorProductIK =
                        Vector.vectorTensorProduct Field.realField vectorI vectorK

                    vectorTensorProductJK =
                        Vector.vectorTensorProduct Field.realField vectorJ vectorK

                    vectorSumTensorProductIKJK =
                        Vector.addVectors Field.realField vectorTensorProductIK vectorTensorProductJK
                in
                Expect.true "vectors equal" (Vector.equal (\valOne valTwo -> Float.Extra.equalWithin 0.1 valOne valTwo) vectorTensorProductIJK vectorSumTensorProductIKJK)
        ]
