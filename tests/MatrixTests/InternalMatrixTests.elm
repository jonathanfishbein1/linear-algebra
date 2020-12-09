module MatrixTests.InternalMatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.Matrix
import List.Extra
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange -10 10)
            "tests matrix findPivot find row with pivot entry"
          <|
            \one two ->
                let
                    m1 =
                        [ Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ one, two ]
                        , Vector.Vector [ two, two ]
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot Vector.realVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange -10 10)
            "tests matrix findPivot find row with pivot entry two"
          <|
            \one two ->
                let
                    m1 =
                        [ Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ 0, 0 ]
                        , Vector.Vector [ one, two ]
                        , Vector.Vector [ two, one ]
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot Vector.realVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 2

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange -10 10)
            "tests matrix findPivotComplex find row with pivot entry"
          <|
            \one two ->
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

                    m1 =
                        [ Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                        , Vector.Vector [ complexNumberOne, complexNumberTwo ]
                        , Vector.Vector [ complexNumberTwo, complexNumberTwo ]
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot Vector.complexVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange -10 10)
            "tests matrix findPivotComplex find row with pivot entry two"
          <|
            \one two ->
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

                    m1 =
                        [ Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                        , Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.zero ]
                        , Vector.Vector [ complexNumberOne, complexNumberTwo ]
                        , Vector.Vector [ complexNumberTwo, complexNumberTwo ]
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot Vector.complexVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 2

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange -10 10)
            "tests matrix scale scales first element to one"
          <|
            \one two ->
                let
                    row =
                        Vector.Vector [ one, two ]

                    (Vector.Vector scaledRow) =
                        Internal.Matrix.scale Vector.realVectorSpace 0 row

                    firstElement =
                        List.Extra.getAt 0 scaledRow
                in
                case firstElement of
                    Just element ->
                        Expect.within (Expect.Absolute 0.000000001) element 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange -10 10)
            "tests matrix complex scale scales first element to one"
          <|
            \one two ->
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

                    row =
                        Vector.Vector [ complexNumberOne, complexNumberTwo ]

                    (Vector.Vector scaledComplexRow) =
                        Internal.Matrix.scale Vector.complexVectorSpace 0 row

                    firstElement =
                        List.Extra.getAt 0 scaledComplexRow
                in
                case firstElement of
                    Just element ->
                        Expect.true "ComplexNumber is equal to one " (ComplexNumbers.equal element ComplexNumbers.one)

                    Nothing ->
                        Expect.fail "error"
        , Test.test
            "tests matrix scale scales empty Vector"
          <|
            \_ ->
                let
                    row =
                        Vector.Vector []

                    (Vector.Vector scaledRow) =
                        Internal.Matrix.scale Vector.realVectorSpace 0 row
                in
                Expect.equal (Vector.Vector scaledRow) Vector.empty
        , Test.test
            "tests matrix complex scale scales empty Vector"
          <|
            \_ ->
                let
                    row =
                        Vector.Vector []

                    (Vector.Vector scaledComplexRow) =
                        Internal.Matrix.scale Vector.complexVectorSpace 0 row
                in
                Expect.equal (Vector.Vector scaledComplexRow) Vector.empty

        -- , Test.test
        --     "tests matrix complex scale scales Vector"
        --   <|
        --     \_ ->
        --         let
        --             complexNumberOne =
        --                 ComplexNumbers.ComplexNumber
        --                     (ComplexNumbers.Real
        --                         0.000001
        --                     )
        --                     (ComplexNumbers.Imaginary
        --                         0.000001
        --                     )
        --             complexNumberTwo =
        --                 ComplexNumbers.ComplexNumber
        --                     (ComplexNumbers.Real
        --                         0.000001
        --                     )
        --                     (ComplexNumbers.Imaginary
        --                         0.000001
        --                     )
        --             row =
        --                 Vector.Vector [ complexNumberOne, complexNumberTwo ]
        --             (Vector.Vector scaledComplexRow) =
        --                 Internal.Matrix.scale Vector.complexVectorSpace 0 row
        --             secondElement =
        --                 ComplexNumbers.divide complexNumberTwo complexNumberOne
        --         in
        --         Expect.equal (Vector.Vector scaledComplexRow) (Vector.Vector [ ComplexNumbers.one, secondElement ])
        -- , Test.fuzz2
        --     (Fuzz.floatRange 1 10)
        --     (Fuzz.floatRange -10 10)
        --     "tests matrix scale scales second element by first"
        --   <|
        --     \one two ->
        --         let
        --             row =
        --                 Vector.Vector [ one, two ]
        --             (Vector.Vector scaledRow) =
        --                 Internal.Matrix.scale Vector.realVectorSpace 0 row
        --             secondElement =
        --                 List.Extra.getAt 1 scaledRow
        --         in
        --         case secondElement of
        --             Just element ->
        --                 Expect.within (Expect.Absolute 0.000000001) element (two / one)
        --             Nothing ->
        --                 Expect.fail "error"
        -- , Test.fuzz2
        --     (Fuzz.floatRange 1 10)
        --     (Fuzz.floatRange -10 10)
        --     "tests matrix subrow has zero under pivot entry"
        --   <|
        --     \one two ->
        --         let
        --             currentRow =
        --                 Vector.Vector [ one, two ]
        --             nextRow =
        --                 Vector.Vector [ two, two ]
        --             (Vector.Vector subRow) =
        --                 Internal.Matrix.subtractRow Vector.realVectorSpace 0 (Internal.Matrix.scale Vector.realVectorSpace 0 currentRow) nextRow
        --             firstElementSecondRow =
        --                 List.Extra.getAt 0 subRow
        --         in
        --         case firstElementSecondRow of
        --             Just element ->
        --                 Expect.within (Expect.Absolute 0.000000001) element 0
        --             Nothing ->
        --                 Expect.fail "error"
        -- , Test.fuzz2
        --     (Fuzz.floatRange 1 10)
        --     (Fuzz.floatRange -10 10)
        --     "tests matrix subtractComplexRow has zero under pivot entry"
        --   <|
        --     \one two ->
        --         let
        --             complexNumberOne =
        --                 ComplexNumbers.ComplexNumber
        --                     (ComplexNumbers.Real
        --                         one
        --                     )
        --                     (ComplexNumbers.Imaginary
        --                         two
        --                     )
        --             complexNumberTwo =
        --                 ComplexNumbers.ComplexNumber
        --                     (ComplexNumbers.Real
        --                         one
        --                     )
        --                     (ComplexNumbers.Imaginary
        --                         two
        --                     )
        --             currentRow =
        --                 Vector.Vector [ complexNumberOne, complexNumberTwo ]
        --             nextRow =
        --                 Vector.Vector [ complexNumberTwo, complexNumberTwo ]
        --             (Vector.Vector subRow) =
        --                 Internal.Matrix.subtractRow Vector.complexVectorSpace 0 (Internal.Matrix.scale Vector.complexVectorSpace 0 currentRow) nextRow
        --             firstElementSecondRow =
        --                 List.Extra.getAt 0 subRow
        --         in
        --         case firstElementSecondRow of
        --             Just element ->
        --                 Expect.true "equal" (ComplexNumbers.equal element ComplexNumbers.zero)
        --             Nothing ->
        --                 Expect.fail "error"
        ]
