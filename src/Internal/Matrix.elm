module Internal.Matrix exposing
    ( diagonal
    , findPivot
    , reduceRow
    , reduceRowBackwards
    , scale
    , subtractRow
    )

import List.Extra
import Vector


{-| Internal function for finding pivot entry in Gaussian elimination
-}
findPivot : List (Vector.Vector number) -> Int -> Maybe Int
findPivot listOfRowVectors initialRowIndex =
    List.Extra.find
        (\currentRowIndexIteration ->
            List.Extra.getAt currentRowIndexIteration listOfRowVectors
                |> Maybe.andThen (\(Vector.Vector currentRowIteration) -> List.Extra.getAt initialRowIndex currentRowIteration)
                |> Maybe.withDefault 0
                |> (/=) 0
        )
        (List.range initialRowIndex (List.length listOfRowVectors - 1))


{-| Internal function for subtracting rows from each other
-}
subtractRow : Int -> Vector.Vector Float -> Vector.Vector Float -> Vector.Vector Float
subtractRow r currentRow (Vector.Vector nextRow) =
    let
        k =
            Maybe.withDefault 1 (List.Extra.getAt r nextRow)

        (Vector.Vector subtractedRow) =
            Vector.map ((*) k) currentRow
                |> Vector.subtractRealVectors (Vector.Vector nextRow)

        firstElement =
            subtractedRow
                |> List.Extra.find
                    ((/=) 0)
                |> Maybe.withDefault 1

        scaledRow =
            List.map (\x -> x / firstElement) subtractedRow
    in
    scaledRow
        |> Vector.Vector


{-| Internal function for scalling rows by pivot entry
-}
scale : Int -> Vector.Vector Float -> Vector.Vector Float
scale rowIndex (Vector.Vector rowVector) =
    case rowVector of
        [] ->
            Vector.Vector []

        xs ->
            let
                elementAtRowIndex =
                    Maybe.withDefault 1 (List.Extra.getAt rowIndex xs)
            in
            Vector.map (\rowElement -> rowElement / elementAtRowIndex) (Vector.Vector xs)


reduceRow : Int -> List (Vector.Vector Float) -> List (Vector.Vector Float)
reduceRow rowIndex listOfVectors =
    let
        firstPivot =
            findPivot listOfVectors rowIndex
    in
    case firstPivot of
        Just fPivot ->
            let
                swappedListOfVectors =
                    List.Extra.swapAt rowIndex fPivot listOfVectors

                scaledRow =
                    List.Extra.getAt rowIndex swappedListOfVectors
                        |> Maybe.map (\vector -> scale rowIndex vector)
                        |> Maybe.withDefault (Vector.Vector [])

                nextRows =
                    List.drop (rowIndex + 1) listOfVectors
                        |> List.map
                            (\vector ->
                                subtractRow rowIndex scaledRow vector
                            )

                newMatrixReduceRow =
                    List.take rowIndex swappedListOfVectors
                        ++ [ scaledRow ]
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
                            |> Maybe.andThen (\(Vector.Vector list) -> List.Extra.findIndex (\x -> x /= 0) list)
                            |> Maybe.withDefault rowIndex

                    scaledRow =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.map (\vector -> scale nextNonZero vector)
                            |> Maybe.withDefault (Vector.Vector [])

                    nextRows =
                        List.drop nextNonZero listOfVectors
                            |> List.map
                                (\vector ->
                                    subtractRow nextNonZero scaledRow vector
                                )

                    newMatrixReduceRow =
                        List.take rowIndex listOfVectors
                            ++ [ scaledRow ]
                            ++ nextRows
                in
                newMatrixReduceRow


reduceRowBackwards : Int -> List (Vector.Vector Float) -> List (Vector.Vector Float)
reduceRowBackwards rowIndex listOfVectors =
    let
        row =
            Maybe.withDefault (Vector.Vector []) (List.Extra.getAt rowIndex listOfVectors)

        prevRows =
            List.take rowIndex listOfVectors
                |> List.map
                    (\vector ->
                        subtractRow rowIndex row vector
                    )
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
