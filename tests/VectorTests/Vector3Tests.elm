module VectorTests.Vector3Tests exposing (suite)

import Expect
import Field
import Fuzz
import Real
import Test
import Vector
import Vector3


suite : Test.Test
suite =
    Test.describe "The Vector3 module"
        [ Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests cross product is orthagonal to both vectors"
          <|
            \one two three ->
                let
                    a =
                        Vector3.Vector3 one two three

                    b =
                        Vector3.Vector3 two three one

                    (Field.Field commutativeDivisionRing) =
                        Real.field

                    aCrossB =
                        Vector3.cross commutativeDivisionRing a b
                            |> Vector3.vector3ToVector

                    aDotACrossB =
                        Vector.dotProduct Real.field (Vector3.vector3ToVector a) aCrossB

                    bDotACrossB =
                        Vector.dotProduct Real.field (Vector3.vector3ToVector b) aCrossB

                    result =
                        Real.equal.eq Real.zero aDotACrossB && Real.equal.eq Real.zero bDotACrossB
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
                        Vector3.Vector3 one two three

                    b =
                        Vector3.Vector3 two three one

                    aCrossB =
                        Vector3.cross Real.commutativeDivisionRing a b
                            |> Vector3.vector3ToVector

                    aVector =
                        Vector3.vector3ToVector a

                    bVector =
                        Vector3.vector3ToVector b

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
        ]
