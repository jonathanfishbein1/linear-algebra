module InternalMatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Float.Extra
import Fuzz
import Internal.Matrix
import List.Extra
import Monoid
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix findPivot find row with pivot entry" <|
            \one two ->
                let
                    m1 =
                        [ Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ one, two ]
                        , Vector.Vector [ two, two ]
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix findPivot find row with pivot entry two" <|
            \one two ->
                let
                    m1 =
                        [ Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ one, two ]
                        , Vector.Vector [ two, one ]
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 2

                    Nothing ->
                        Expect.fail "error"
        ]
