module Internal.Matrix exposing
    ( diagonal
    , findPivot
    , map2VectorCartesian
    , map2VectorCartesianComplex
    , reduceRowBackwards
    , scale
    , subtractRow
    , upperTriangle
    )

import ComplexNumbers
import List.Extra
import Vector


{-| Internal function for finding pivot entry in Gaussian elimination
-}
findPivot : List (Vector.Vector number) -> Int -> Maybe Int
findPivot listOfRowVectors initialRowIndex =
    List.Extra.find
        (\currentRowIndexIteration ->
            List.Extra.getAt currentRowIndexIteration listOfRowVectors
                |> Maybe.andThen (Vector.getAt initialRowIndex)
                |> Maybe.withDefault 0
                |> (/=) 0
        )
        (List.range initialRowIndex (List.length listOfRowVectors - 1))


{-| Internal function for subtracting rows from each other
-}
subtractRow : Int -> Vector.Vector Float -> Vector.Vector Float -> Vector.Vector Float
subtractRow r currentRow nextRow =
    Vector.getAt r nextRow
        |> Maybe.andThen
            (\nElement ->
                Vector.getAt r currentRow
                    |> Maybe.map
                        (\currentElement ->
                            (if currentElement == 0 then
                                Vector.map ((*) nElement) currentRow

                             else
                                Vector.map ((*) (nElement / currentElement)) currentRow
                            )
                                |> Vector.subtractRealVectors nextRow
                        )
            )
        |> Maybe.withDefault nextRow


{-| Internal function for scalling rows by pivot entry
-}
scale : Int -> Vector.Vector Float -> Vector.Vector Float
scale rowIndex rowVector =
    Vector.getAt rowIndex rowVector
        |> Maybe.map
            (\elementAtRowIndex ->
                Vector.map
                    (\rowElement ->
                        if elementAtRowIndex == 0 then
                            rowElement

                        else
                            rowElement / elementAtRowIndex
                    )
                    rowVector
            )
        |> Maybe.withDefault rowVector


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


diagonal : Int -> Int -> number
diagonal columnIndex rowIndex =
    if columnIndex == rowIndex then
        1

    else
        0


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


map2VectorCartesianComplex : List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number))
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


upperTriangle : Int -> List (Vector.Vector Float) -> List (Vector.Vector Float)
upperTriangle rowIndex listOfVectors =
    let
        firstPivot =
            findPivot listOfVectors rowIndex
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
