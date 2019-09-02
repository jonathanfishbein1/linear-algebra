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
import Internal.Field
import List.Extra
import Vector


{-| Internal function for finding pivot entry in Gaussian elimination
-}
findPivotGeneric : Vector.VectorSpace a -> List (Vector.Vector a) -> Int -> Maybe Int
findPivotGeneric { abelianGroup } listOfRowVectors initialRowIndex =
    List.Extra.find
        (\currentRowIndexIteration ->
            List.Extra.getAt currentRowIndexIteration listOfRowVectors
                |> Maybe.andThen (Vector.getAt initialRowIndex)
                |> Maybe.withDefault abelianGroup.field.zero
                |> (/=) abelianGroup.field.zero
        )
        (List.range initialRowIndex (List.length listOfRowVectors - 1))


findPivotReal : List (Vector.Vector Float) -> Int -> Maybe Int
findPivotReal =
    findPivotGeneric Vector.realVectorSpace


findPivotComplex : List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> Int -> Maybe Int
findPivotComplex =
    findPivotGeneric Vector.complexVectorSpace


subtractRowGeneric : Vector.VectorSpace a -> Int -> Vector.Vector a -> Vector.Vector a -> Vector.Vector a
subtractRowGeneric { abelianGroup } r currentRow nextRow =
    Vector.getAt r nextRow
        |> Maybe.andThen
            (\nElement ->
                Vector.getAt r currentRow
                    |> Maybe.map
                        (\currentElement ->
                            (if currentElement == abelianGroup.field.zero then
                                currentRow

                             else
                                Vector.map (abelianGroup.field.multiply (abelianGroup.field.divide nElement currentElement)) currentRow
                            )
                                |> abelianGroup.subtractVectors nextRow
                        )
            )
        |> Maybe.withDefault nextRow


{-| Internal function for subtracting rows from each other
-}
subtractRow : Int -> Vector.Vector Float -> Vector.Vector Float -> Vector.Vector Float
subtractRow r currentRow nextRow =
    subtractRowGeneric Vector.realVectorSpace r currentRow nextRow


{-| Internal function for subtracting complex rows from each other
-}
subtractComplexRow : Int -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)
subtractComplexRow r currentRow nextRow =
    subtractRowGeneric Vector.complexVectorSpace r currentRow nextRow


{-| Internal function for scalling rows by pivot entry
-}
scaleGeneric : Vector.VectorSpace a -> Int -> Vector.Vector a -> Vector.Vector a
scaleGeneric { abelianGroup } rowIndex rowVector =
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


{-| Internal function for scalling rows by pivot entry
-}
scale : Int -> Vector.Vector Float -> Vector.Vector Float
scale rowIndex rowVector =
    scaleGeneric Vector.realVectorSpace rowIndex rowVector


{-| Internal function for scalling rows by pivot entry
-}
scaleComplex : Int -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float) -> Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)
scaleComplex rowIndex rowVector =
    scaleGeneric Vector.complexVectorSpace rowIndex rowVector


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


diagonalGeneric : Internal.Field.Field a -> Int -> Int -> a
diagonalGeneric { zero, one } columnIndex rowIndex =
    if columnIndex == rowIndex then
        one

    else
        zero


diagonal : Int -> Int -> Float
diagonal columnIndex rowIndex =
    diagonalGeneric Internal.Field.realField columnIndex rowIndex


diagonalComplex : Int -> Int -> ComplexNumbers.ComplexNumberCartesian Float
diagonalComplex columnIndex rowIndex =
    diagonalGeneric Internal.Field.complexField columnIndex rowIndex


map2VectorCartesianGeneric : Vector.InnerProductSpace a -> List (Vector.Vector a) -> List (Vector.Vector a) -> List (Vector.Vector a)
map2VectorCartesianGeneric { innerProduct } left right =
    List.foldl
        (\leftVector finalAcc ->
            finalAcc
                ++ [ List.foldl (\rightVector intermediateAcc -> intermediateAcc ++ [ innerProduct leftVector rightVector ]) [] right
                        |> Vector.Vector
                   ]
        )
        []
        left


map2VectorCartesian : List (Vector.Vector Float) -> List (Vector.Vector Float) -> List (Vector.Vector Float)
map2VectorCartesian left right =
    map2VectorCartesianGeneric Vector.realInnerProductSpace left right


map2VectorCartesianComplex : List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float)) -> List (Vector.Vector (ComplexNumbers.ComplexNumberCartesian Float))
map2VectorCartesianComplex left right =
    map2VectorCartesianGeneric Vector.complexInnerProductSpace left right


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
