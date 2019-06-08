module Internal.Matrix exposing
    ( diagonal
    , findPivot
    , map2VectorCartesian
    , map2VectorCartesianComplex
    , reduceRow
    , reduceRowBackwards
    , scale
    , subtractRow
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
    let
        k =
            Maybe.withDefault 1 (Vector.getAt r nextRow)

        (Vector.Vector subtractedRow) =
            Vector.map ((*) k) currentRow
                |> Vector.subtractRealVectors nextRow

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
scale rowIndex rowVector =
    Vector.getAt rowIndex rowVector
        |> Maybe.map (\elementAtRowIndex -> Vector.map (\rowElement -> rowElement / elementAtRowIndex) rowVector)
        |> Maybe.withDefault rowVector


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
                        |> Maybe.map (scale rowIndex)
                        |> Maybe.withDefault (Vector.Vector [])

                nextRows =
                    List.drop (rowIndex + 1) listOfVectors
                        |> List.map
                            (subtractRow rowIndex scaledRow)

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
                            |> Maybe.andThen (\(Vector.Vector list) -> List.Extra.findIndex ((/=) 0) list)
                            |> Maybe.withDefault rowIndex

                    scaledRow =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.map (scale nextNonZero)
                            |> Maybe.withDefault (Vector.Vector [])

                    nextRows =
                        List.drop nextNonZero listOfVectors
                            |> List.map
                                (subtractRow nextNonZero scaledRow)

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


map2VectorCartesian : List (Vector.Vector number) -> Vector.Vector number -> List (Vector.Vector number) -> List (Vector.Vector number) -> List (Vector.Vector number) -> List (Vector.Vector number)
map2VectorCartesian right (Vector.Vector intermediateList) acc left currentRight =
    case ( left, currentRight ) of
        ( l :: _, r :: rs ) ->
            map2VectorCartesian right (Vector.Vector (intermediateList ++ [ Vector.realVectorDotProduct l r ])) acc left rs

        ( _ :: ls, [] ) ->
            map2VectorCartesian right (Vector.Vector []) (acc ++ [ Vector.Vector <| intermediateList ]) ls right

        ( [], _ ) ->
            acc


map2VectorCartesianComplex : List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian number) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian number))
map2VectorCartesianComplex right (Vector.Vector intermediateList) acc left currentRight =
    case ( left, currentRight ) of
        ( l :: _, r :: rs ) ->
            map2VectorCartesianComplex right (Vector.Vector (intermediateList ++ [ Vector.complexVectorDotProduct l r ])) acc left rs

        ( _ :: ls, [] ) ->
            map2VectorCartesianComplex right (Vector.Vector []) (acc ++ [ Vector.Vector <| intermediateList ]) ls right

        ( [], _ ) ->
            acc
