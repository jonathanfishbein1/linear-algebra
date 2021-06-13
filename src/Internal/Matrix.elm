module Internal.Matrix exposing
    ( calculateUpperTriangularFormRectangle
    , diagonal
    , findPivot
    , map2VectorCartesian
    , reduceRowBackwards
    , scale
    , subtractRow
    )

import AbelianGroup
import CommutativeDivisionRing
import Field
import List.Extra
import RowVector
import Vector


{-| Internal function for finding pivot entry in Gaussian elimination
-}
findPivot : RowVector.VectorSpace a -> List (RowVector.RowVector a) -> Int -> Maybe Int
findPivot { field } listOfRowVectors initialRowIndex =
    let
        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition

        monoid =
            group.monoid
    in
    List.Extra.find
        (\currentRowIndexIteration ->
            List.Extra.getAt currentRowIndexIteration listOfRowVectors
                |> Maybe.andThen (RowVector.getAt initialRowIndex)
                |> Maybe.map ((/=) monoid.identity)
                |> Maybe.withDefault False
        )
        (List.range initialRowIndex (List.length listOfRowVectors - 1))


subtractRow :
    RowVector.VectorSpace a
    -> Int
    -> RowVector.RowVector a
    -> RowVector.RowVector a
    -> RowVector.RowVector a
subtractRow { abelianGroup, field } r currentRow nextRow =
    let
        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            field

        (AbelianGroup.AbelianGroup groupAddition) =
            commutativeDivisionRing.addition

        (AbelianGroup.AbelianGroup vectorGroup) =
            abelianGroup
    in
    RowVector.getAt r nextRow
        |> Maybe.andThen
            (\nElement ->
                RowVector.getAt r currentRow
                    |> Maybe.map
                        (\currentElement ->
                            vectorGroup.inverse
                                (if currentElement == groupAddition.monoid.identity then
                                    currentRow

                                 else
                                    RowVector.scalarMultiplication
                                        field
                                        (commutativeDivisionRing.multiplication.monoid.semigroup nElement (commutativeDivisionRing.multiplication.inverse currentElement))
                                        currentRow
                                )
                                |> vectorGroup.monoid.semigroup nextRow
                        )
            )
        |> Maybe.withDefault nextRow


{-| Internal function for scalling rows by pivot entry
-}
scale : RowVector.VectorSpace a -> Int -> RowVector.RowVector a -> RowVector.RowVector a
scale { field } rowIndex rowVector =
    let
        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            field

        (AbelianGroup.AbelianGroup groupAddition) =
            commutativeDivisionRing.addition
    in
    RowVector.getAt rowIndex rowVector
        |> Maybe.map
            (\elementAtRowIndex ->
                RowVector.map
                    (\rowElement ->
                        if elementAtRowIndex == groupAddition.monoid.identity then
                            rowElement

                        else
                            commutativeDivisionRing.multiplication.monoid.semigroup rowElement (commutativeDivisionRing.multiplication.inverse elementAtRowIndex)
                    )
                    rowVector
            )
        |> Maybe.withDefault rowVector


reduceRowBackwards :
    RowVector.VectorSpace a
    -> Int
    -> List (RowVector.RowVector a)
    -> List (RowVector.RowVector a)
reduceRowBackwards vectorSpace rowIndex listOfVectors =
    let
        row =
            Maybe.withDefault
                RowVector.empty
                (List.Extra.getAt rowIndex listOfVectors)

        prevRows =
            List.take rowIndex listOfVectors
                |> List.map
                    (subtractRow vectorSpace rowIndex row)
    in
    List.concat
        [ prevRows
        , [ row ]
        , List.drop (rowIndex + 1) listOfVectors
        ]


diagonal : CommutativeDivisionRing.CommutativeDivisionRing a -> Int -> Int -> a
diagonal (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing) columnIndex rowIndex =
    let
        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition

        additionMonoid =
            group.monoid

        multiplicationMonoid =
            commutativeDivisionRing.multiplication.monoid
    in
    if columnIndex == rowIndex then
        multiplicationMonoid.identity

    else
        additionMonoid.identity


map2VectorCartesian :
    RowVector.InnerProductSpace a
    -> List (RowVector.RowVector a)
    -> List (RowVector.RowVector a)
    -> List (RowVector.RowVector a)
map2VectorCartesian { innerProduct } left right =
    List.foldl
        (\leftVector finalAcc ->
            finalAcc
                ++ [ List.foldl
                        (\rightVector intermediateAcc -> intermediateAcc ++ [ innerProduct leftVector rightVector ])
                        []
                        right
                        |> Vector.Vector
                        |> RowVector.RowVector
                   ]
        )
        []
        left


calculateUpperTriangularFormRectangle :
    RowVector.VectorSpace a
    -> Int
    -> List (RowVector.RowVector a)
    -> List (RowVector.RowVector a)
calculateUpperTriangularFormRectangle vectorSpace rowIndex listOfVectors =
    let
        firstPivot =
            findPivot vectorSpace listOfVectors rowIndex

        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            vectorSpace.field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition

        monoid =
            group.monoid
    in
    case firstPivot of
        Just fPivot ->
            let
                swappedListOfVectors =
                    List.Extra.swapAt rowIndex fPivot listOfVectors

                currentRow =
                    List.Extra.getAt rowIndex swappedListOfVectors
                        |> Maybe.withDefault RowVector.empty

                nextRows =
                    List.drop (rowIndex + 1) swappedListOfVectors
                        |> List.map
                            (subtractRow vectorSpace rowIndex currentRow)

                newMatrixReduceRow =
                    List.concat
                        [ List.take rowIndex swappedListOfVectors
                        , [ currentRow ]
                        , nextRows
                        ]
            in
            newMatrixReduceRow

        Nothing ->
            if rowIndex == (List.length listOfVectors - 1) then
                listOfVectors

            else
                let
                    nextNonZero =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.andThen
                                (RowVector.findIndex ((/=) monoid.identity))
                            |> Maybe.withDefault rowIndex

                    currentRow =
                        List.Extra.getAt rowIndex listOfVectors
                            |> Maybe.withDefault RowVector.empty

                    nextRows =
                        List.drop nextNonZero listOfVectors
                            |> List.map
                                (subtractRow vectorSpace nextNonZero currentRow)

                    newMatrixReduceRow =
                        List.concat
                            [ List.take rowIndex listOfVectors
                            , [ currentRow ]
                            , nextRows
                            ]
                in
                newMatrixReduceRow
