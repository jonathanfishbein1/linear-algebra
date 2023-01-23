module VectorTests.Vector3Tests exposing (suite)

import Expect
import Field
import Fuzz
import Internal.Vector
import Real
import Test
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
                        Internal.Vector.dotProduct Real.field (Vector3.vector3ToVector a) aCrossB

                    bDotACrossB =
                        Internal.Vector.dotProduct Real.field (Vector3.vector3ToVector b) aCrossB

                    result =
                        Real.equal.eq Real.zero aDotACrossB && Real.equal.eq Real.zero bDotACrossB
                in
                if result then
                    Expect.pass

                else
                    Expect.fail "a X b is not orthagonal to both a and b"
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
                        Internal.Vector.lengthReal aVector

                    bLength =
                        Internal.Vector.lengthReal bVector

                    aCrossBLength =
                        Internal.Vector.lengthReal aCrossB

                    angle =
                        Internal.Vector.angleBetween aVector bVector
                in
                if Real.equal.eq aCrossBLength (Real.multiply (Real.multiply aLength bLength) (Real.map Basics.sin angle)) then
                    Expect.pass

                else
                    Expect.fail "length of cross product is not the length of the two vectors times the sin of the angle between them"
        ]
