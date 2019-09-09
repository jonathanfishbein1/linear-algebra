module Internal.Matrix exposing
    ( calculateUpperTriangularFormRectangle
    , diagonal
    , findPivot
    , map2VectorCartesian
    , reduceRowBackwards
    , scale
    , subtractRow
    )

import ComplexNumbers
import Field
import List.Extra
import Vector


{-| Internal function for finding pivot entry in Gaussian elimination
-}
findPivot : Vector.VectorSpace a -> List (Vector.Vector a) -> Int -> Maybe Int
findPivot { abelianGroup } listOfRowVectors initialRowIndex =
    List.Extra.find
        (\currentRowIndexIteration ->
            List.Extra.getAt currentRowIndexIteration listOfRowVectors
                |> Maybe.andThen (Vector.getAt initialRowIndex)
                |> Maybe.withDefault abelianGroup.field.zero
                |> (/=) abelianGroup.field.zero
        )
        (List.range initialRowIndex (List.length listOfRowVectors - 1))


subtractRow : Vector.VectorSpace a -> Int -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
subtractRow { abelianGroup } r currentRow nextRow =
    Vector.getAt r nextRow
        |> Maybe.andThen
            (\nElement ->
                Vector.getAt r currentRow
                    |> Maybe.map
                        (\currentElement ->
                            (if currentElement == abelianGroup.field.zero then
                                currentRow

                             else
                                Vector.scalarMultiplication abelianGroup.field (abelianGroup.field.divide nElement currentElement) currentRow
                            )
                                |> abelianGroup.subtractVects nextRow
                        )
            )
        |> Maybe.withDefault nextRow


{-| Internal function for scalling rows by pivot entry
-}
scale : Vector.VectorSpace a -> Int -> Vector.Vector a -> Vector.Vector a
scale { abelianGroup } rowIndex rowVector =
    Vector.getAt rowIndex rowVector
        |> Maybe.map
            (\elementAtRowIndex ->
                Vector.map
                    (\rowElement ->
                        if elementAtRowIndex == abelianGroup.field.zero then
                            rowElement

                        else
                            abelianGroup.field.divide rowElement elementAtRowIndex
                    )
                    rowVector
            )
        |> Maybe.withDefault rowVector


reduceRowBackwards : Vector.VectorSpace a -> Int -> List (Vector.Vector a) -> List (Vector.Vector a)
reduceRowBackwards vectorSpace rowIndex listOfVectors =
    let
        row =
            Maybe.withDefault (Vector.Vector []) (List.Extra.getAt rowIndex listOfVectors)

        prevRows =
            List.take rowIndex listOfVectors
                |> List.map
                    (subtractRow vectorSpace rowIndex row)
    in
    prevRows
        ++ [ row ]
        ++ List.drop (rowIndex + 1) listOfVectors


diagonal : Field.Field a -> Int -> Int -> a
diagonal { zero, one } columnIndex rowIndex =
    if columnIndex == rowIndex then
        one

    else
        zero


map2VectorCartesian : Vector.InnerProductSpace a -> List (Vector.Vector a) -> List (Vector.Vector a) -> List (Vector.Vector a)
map2VectorCartesian { innerProduct } left right =
    List.foldl
        (\leftVector finalAcc ->
            finalAcc
                ++ [ List.foldl (\rightVector intermediateAcc -> intermediateAcc ++ [ innerProduct leftVector rightVector ]) [] right
                        |> Vector.Vector
                   ]
        )
        []
        left


calculateUpperTriangularFormRectangle : Vector.VectorSpace a -> Int -> List (Vector.Vector a) -> List (Vector.Vector a)
calculateUpperTriangularFormRectangle vectorSpace rowIndex listOfVectors =
    let
        firstPivot =
            findPivot vectorSpace listOfVectors rowIndex
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
                                        subtractRow vectorSpace rowIndex currentRow row
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
                            |> Maybe.andThen (Vector.findIndex ((/=) vectorSpace.abelianGroup.field.zero))
                            |> Maybe.withDefault rowIndex

                    currentRow =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.withDefault (Vector.Vector [])

                    nextRows =
                        List.drop nextNonZero listOfVectors
                            |> List.map
                                (subtractRow vectorSpace nextNonZero currentRow)

                    newMatrixReduceRow =
                        List.take rowIndex listOfVectors
                            ++ [ currentRow ]
                            ++ nextRows
                in
                newMatrixReduceRow
