module InternalMatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.Matrix
import List.Extra
import Matrix
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
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scale scales first element to one" <|
            \one two ->
                let
                    row =
                        Vector.Vector [ one, two ]

                    (Vector.Vector scaledRow) =
                        Internal.Matrix.scale 0 row

                    firstElement =
                        List.Extra.getAt 0 scaledRow
                in
                case firstElement of
                    Just element ->
                        Expect.equal element 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix scale scales second element by first" <|
            \one two ->
                let
                    row =
                        Vector.Vector [ one, two ]

                    (Vector.Vector scaledRow) =
                        Internal.Matrix.scale 0 row

                    secondElement =
                        List.Extra.getAt 1 scaledRow
                in
                case secondElement of
                    Just element ->
                        Expect.within (Expect.Absolute 0.000000001) element (two / one)

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange 1 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests matrix subrow has zero under pivot entry" <|
            \one two ->
                let
                    currentRow =
                        Vector.Vector [ one, two ]

                    nextRow =
                        Vector.Vector [ two, two ]

                    (Vector.Vector subRow) =
                        Internal.Matrix.subtractRow 0 (Internal.Matrix.scale 0 currentRow) nextRow

                    firstElementSecondRow =
                        List.Extra.getAt 0 subRow
                in
                case firstElementSecondRow of
                    Just element ->
                        Expect.within (Expect.Absolute 0.000000001) element 0

                    Nothing ->
                        Expect.fail "error"
        ]
