module Internal.Matrix exposing
    ( calculateUpperTriangularFormRectangle
    , calculateUpperTriangularFormRectangleComplex
    , diagonal
    , diagonalComplex
    , findPivotComplex
    , findPivotReal
    , map2VectorCartesian
    , map2VectorCartesianComplex
    , reduceRowBackwards
    , reduceRowBackwardsComplex
    , scale
    , scaleComplex
    , subtractComplexRow
    , subtractRow
    )

import ComplexNumbers
import Internal.Algebra
import List.Extra
import Vector


{-| Internal function for finding pivot entry in Gaussian elimination
-}
findPivotGeneric : Internal.Algebra.Algebra a -> List (Vector.Vector a) -> Int -> Maybe Int
findPivotGeneric { zero } listOfRowVectors initialRowIndex =
    List.Extra.find
        (\currentRowIndexIteration ->
            List.Extra.getAt currentRowIndexIteration listOfRowVectors
                |> Maybe.andThen (Vector.getAt initialRowIndex)
                |> Maybe.withDefault zero
                |> (/=) zero
        )
        (List.range initialRowIndex (List.length listOfRowVectors - 1))


findPivotReal : List (Vector.Vector Float) -> Int -> Maybe Int
findPivotReal =
    findPivotGeneric Internal.Algebra.realAlgebra


findPivotComplex : List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> Int -> Maybe Int
findPivotComplex =
    findPivotGeneric Internal.Algebra.complexAlgebra


subtractRowGeneric : Internal.Algebra.Algebra a -> Int -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
subtractRowGeneric { zero, multiply, divide, subtractVectors } r currentRow nextRow =
    Vector.getAt r nextRow
        |> Maybe.andThen
            (\nElement ->
                Vector.getAt r currentRow
                    |> Maybe.map
                        (\currentElement ->
                            (if currentElement == zero then
                                currentRow

                             else
                                Vector.map (multiply (divide nElement currentElement)) currentRow
                            )
                                |> subtractVectors nextRow
                        )
            )
        |> Maybe.withDefault nextRow


{-| Internal function for subtracting rows from each other
-}
subtractRow : Int -> Vector.Vector Float -> Vector.Vector Float -> Vector.Vector Float
subtractRow r currentRow nextRow =
    subtractRowGeneric Internal.Algebra.realAlgebra r currentRow nextRow


{-| Internal function for subtracting complex rows from each other
-}
subtractComplexRow : Int -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)
subtractComplexRow r currentRow nextRow =
    subtractRowGeneric Internal.Algebra.complexAlgebra r currentRow nextRow


{-| Internal function for scalling rows by pivot entry
-}
scaleGeneric : Internal.Algebra.Algebra a -> Int -> Vector.Vector a -> Vector.Vector a
scaleGeneric { zero, divide } rowIndex rowVector =
    Vector.getAt rowIndex rowVector
        |> Maybe.map
            (\elementAtRowIndex ->
                Vector.map
                    (\rowElement ->
                        if elementAtRowIndex == zero then
                            rowElement

                        else
                            divide rowElement elementAtRowIndex
                    )
                    rowVector
            )
        |> Maybe.withDefault rowVector


{-| Internal function for scalling rows by pivot entry
-}
scale : Int -> Vector.Vector Float -> Vector.Vector Float
scale rowIndex rowVector =
    scaleGeneric Internal.Algebra.realAlgebra rowIndex rowVector


{-| Internal function for scalling rows by pivot entry
-}
scaleComplex : Int -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)
scaleComplex rowIndex rowVector =
    scaleGeneric Internal.Algebra.complexAlgebra rowIndex rowVector


reduceRowBackwards : Int -> List (Vector.Vector Float) -> List (Vector.Vector Float)
reduceRowBackwards rowIndex listOfVectors =
    let
        row =
            Maybe.withDefault (Vector.Vector []) (List.Extra.getAt rowIndex listOfVectors)

        prevRows =
            List.take rowIndex listOfVectors
                |> List.map
                    (subtractRow rowIndex row)
    in
    prevRows
        ++ [ row ]
        ++ List.drop (rowIndex + 1) listOfVectors


reduceRowBackwardsComplex : Int -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float))
reduceRowBackwardsComplex rowIndex listOfVectors =
    let
        row =
            Maybe.withDefault (Vector.Vector []) (List.Extra.getAt rowIndex listOfVectors)

        prevRows =
            List.take rowIndex listOfVectors
                |> List.map
                    (subtractComplexRow rowIndex row)
    in
    prevRows
        ++ [ row ]
        ++ List.drop (rowIndex + 1) listOfVectors


diagonal : Int -> Int -> number
diagonal columnIndex rowIndex =
    if columnIndex == rowIndex then
        1

    else
        0


diagonalComplex : Int -> Int -> ComplexNumbers.ComplexNumberCartesian Float
diagonalComplex columnIndex rowIndex =
    if columnIndex == rowIndex then
        ComplexNumbers.one

    else
        ComplexNumbers.zero


map2VectorCartesian : List (Vector.Vector number) -> List (Vector.Vector number) -> List (Vector.Vector number)
map2VectorCartesian left right =
    List.foldl
        (\leftVector finalAcc ->
            finalAcc
                ++ [ List.foldl (\rightVector intermediateAcc -> intermediateAcc ++ [ Vector.realVectorDotProduct leftVector rightVector ]) [] right
                        |> Vector.Vector
                   ]
        )
        []
        left


map2VectorCartesianComplex : List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float))
map2VectorCartesianComplex left right =
    List.foldl
        (\leftVector finalAcc ->
            finalAcc
                ++ [ List.foldl (\rightVector intermediateAcc -> intermediateAcc ++ [ Vector.complexVectorDotProduct leftVector rightVector ]) [] right
                        |> Vector.Vector
                   ]
        )
        []
        left


calculateUpperTriangularFormRectangle : Int -> List (Vector.Vector Float) -> List (Vector.Vector Float)
calculateUpperTriangularFormRectangle rowIndex listOfVectors =
    let
        firstPivot =
            findPivotReal listOfVectors rowIndex
    in
    case firstPivot of
        Just fPivot ->
            let
                swappedListOfVectors =
                    List.Extra.swapAt rowIndex fPivot listOfVectors

                currentRow =
                    List.Extra.getAt rowIndex swappedListOfVectors
                        |> Maybe.withDefault (Vector.Vector [])

                nextRows =
                    List.drop (rowIndex + 1) listOfVectors
                        |> List.map
                            (\row ->
                                let
                                    subtractedRow =
                                        subtractRow rowIndex currentRow row
                                in
                                subtractedRow
                            )

                newMatrixReduceRow =
                    List.take rowIndex swappedListOfVectors
                        ++ [ currentRow ]
                        ++ nextRows
            in
            newMatrixReduceRow

        Nothing ->
            if rowIndex == (List.length listOfVectors - 1) then
                listOfVectors

            else
                let
                    nextNonZero =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.andThen (Vector.findIndex ((/=) 0))
                            |> Maybe.withDefault rowIndex

                    currentRow =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.withDefault (Vector.Vector [])

                    nextRows =
                        List.drop nextNonZero listOfVectors
                            |> List.map
                                (subtractRow nextNonZero currentRow)

                    newMatrixReduceRow =
                        List.take rowIndex listOfVectors
                            ++ [ currentRow ]
                            ++ nextRows
                in
                newMatrixReduceRow


calculateUpperTriangularFormRectangleComplex : Int -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float))
calculateUpperTriangularFormRectangleComplex rowIndex listOfVectors =
    let
        firstPivot =
            findPivotComplex listOfVectors rowIndex
    in
    case firstPivot of
        Just fPivot ->
            let
                swappedListOfVectors =
                    List.Extra.swapAt rowIndex fPivot listOfVectors

                currentRow =
                    List.Extra.getAt rowIndex swappedListOfVectors
                        |> Maybe.withDefault (Vector.Vector [])

                nextRows =
                    List.drop (rowIndex + 1) listOfVectors
                        |> List.map
                            (\row ->
                                let
                                    subtractedRow =
                                        subtractComplexRow rowIndex currentRow row
                                in
                                subtractedRow
                            )

                newMatrixReduceRow =
                    List.take rowIndex swappedListOfVectors
                        ++ [ currentRow ]
                        ++ nextRows
            in
            newMatrixReduceRow

        Nothing ->
            if rowIndex == (List.length listOfVectors - 1) then
                listOfVectors

            else
                let
                    nextNonZero =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.andThen (Vector.findIndex ((/=) ComplexNumbers.zero))
                            |> Maybe.withDefault rowIndex

                    currentRow =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.withDefault (Vector.Vector [])

                    nextRows =
                        List.drop nextNonZero listOfVectors
                            |> List.map
                                (subtractComplexRow nextNonZero currentRow)

                    newMatrixReduceRow =
                        List.take rowIndex listOfVectors
                            ++ [ currentRow ]
                            ++ nextRows
                in
                newMatrixReduceRow
