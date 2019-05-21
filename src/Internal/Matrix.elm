module Internal.Matrix exposing
    ( findPivot
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
