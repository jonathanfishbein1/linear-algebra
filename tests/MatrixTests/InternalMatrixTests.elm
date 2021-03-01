module MatrixTests.InternalMatrixTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.Matrix
import Real
import RowVector
import Test
import Vector


suite : Test.Test
suite =
    Test.describe "The LinearAlgebra module"
        [ Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix findPivot find row with pivot entry"
          <|
            \one two ->
                let
                    m1 =
                        [ [ Real.zero, Real.zero ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ one, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ two, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot RowVector.realVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix findPivot find row with pivot entry two"
          <|
            \one two ->
                let
                    m1 =
                        [ [ Real.zero, Real.zero ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ Real.zero, Real.zero ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ one, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ two, one ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot RowVector.realVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 2

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix findPivotComplex find row with pivot entry"
          <|
            \one two ->
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

                    m1 =
                        [ [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ complexNumberOne, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ complexNumberTwo, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot RowVector.complexVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 1

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix findPivotComplex find row with pivot entry two"
          <|
            \one two ->
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

                    m1 =
                        [ [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ ComplexNumbers.zero, ComplexNumbers.zero ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ complexNumberOne, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        , [ complexNumberTwo, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector
                        ]

                    pivotLocation =
                        Internal.Matrix.findPivot RowVector.complexVectorSpace m1 0
                in
                case pivotLocation of
                    Just location ->
                        Expect.equal location 2

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix scale scales first element to one"
          <|
            \one two ->
                let
                    row =
                        [ one, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    scaledRow =
                        Internal.Matrix.scale RowVector.realVectorSpace 0 row

                    firstElement =
                        RowVector.getAt 0 scaledRow
                in
                case firstElement of
                    Just element ->
                        Expect.true "matrix scale scales first element to one " (Real.equal.eq element Real.one)

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix complex scale scales first element to one"
          <|
            \one two ->
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

                    row =
                        [ complexNumberOne, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    scaledComplexRow =
                        Internal.Matrix.scale RowVector.complexVectorSpace 0 row

                    firstElement =
                        RowVector.getAt 0 scaledComplexRow
                in
                case firstElement of
                    Just element ->
                        Expect.true "ComplexNumber is equal to one " (ComplexNumbers.equal.eq element ComplexNumbers.one)

                    Nothing ->
                        Expect.fail "error"
        , Test.test
            "tests matrix scale scales empty Vector"
          <|
            \_ ->
                let
                    row =
                        RowVector.empty

                    scaledRow =
                        Internal.Matrix.scale RowVector.realVectorSpace 0 row
                in
                Expect.equal scaledRow RowVector.empty
        , Test.test
            "tests matrix complex scale scales empty Vector"
          <|
            \_ ->
                let
                    row =
                        RowVector.empty

                    scaledComplexRow =
                        Internal.Matrix.scale RowVector.complexVectorSpace 0 row
                in
                Expect.equal scaledComplexRow RowVector.empty
        , Test.test
            "tests matrix complex scale scales Vector"
          <|
            \_ ->
                let
                    complexNumberOne =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                0.000001
                            )
                            (Imaginary.Imaginary
                                (Real.Real 0.000001)
                            )

                    complexNumberTwo =
                        ComplexNumbers.ComplexNumber
                            (Real.Real
                                0.000001
                            )
                            (Imaginary.Imaginary
                                (Real.Real 0.000001)
                            )

                    row =
                        [ complexNumberOne, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    scaledComplexRow =
                        Internal.Matrix.scale RowVector.complexVectorSpace 0 row

                    secondElement =
                        ComplexNumbers.divide complexNumberTwo complexNumberOne
                in
                Expect.equal scaledComplexRow (RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, secondElement ]))
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix scale scales second element by first"
          <|
            \one two ->
                let
                    row =
                        [ one, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    scaledRow =
                        Internal.Matrix.scale RowVector.realVectorSpace 0 row

                    secondElement =
                        RowVector.getAt 1 scaledRow
                in
                case secondElement of
                    Just element ->
                        Expect.true "matrix scale scales second element by first" (Real.equal.eq element (Real.divide two one))

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix subrow has zero under pivot entry"
          <|
            \one two ->
                let
                    currentRow =
                        [ one, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    nextRow =
                        [ two, two ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    subRow =
                        Internal.Matrix.subtractRow RowVector.realVectorSpace 0 (Internal.Matrix.scale RowVector.realVectorSpace 0 currentRow) nextRow

                    firstElementSecondRow =
                        RowVector.getAt 0 subRow
                in
                case firstElementSecondRow of
                    Just element ->
                        Expect.true "matrix subrow has zero under pivot entry" (Real.equal.eq element Real.zero)

                    Nothing ->
                        Expect.fail "error"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests matrix subtractComplexRow has zero under pivot entry"
          <|
            \one two ->
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

                    currentRow =
                        [ complexNumberOne, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    nextRow =
                        [ complexNumberTwo, complexNumberTwo ]
                            |> Vector.Vector
                            |> RowVector.RowVector

                    subRow =
                        Internal.Matrix.subtractRow RowVector.complexVectorSpace 0 (Internal.Matrix.scale RowVector.complexVectorSpace 0 currentRow) nextRow

                    firstElementSecondRow =
                        RowVector.getAt 0 subRow
                in
                case firstElementSecondRow of
                    Just element ->
                        Expect.true "equal" (ComplexNumbers.equal.eq element ComplexNumbers.zero)

                    Nothing ->
                        Expect.fail "error"
        ]
