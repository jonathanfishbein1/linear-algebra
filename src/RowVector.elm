module RowVector exposing
    ( RowVector(..)
    , all
    , foldl
    , map
    , map2
    , parseRowVector
    )

import Parser exposing ((|.), (|=))
import Vector


{-| Row Vector
-}
type RowVector a
    = RowVector (Vector.Vector a)


map : (a -> b) -> RowVector a -> RowVector b
map f (RowVector vector) =
    Vector.map f vector
        |> RowVector


map2 : (a -> b -> c) -> RowVector a -> RowVector b -> RowVector c
map2 f (RowVector vectorOne) (RowVector vectorTwo) =
    Vector.map2 f vectorOne vectorTwo
        |> RowVector


{-| Left fold over a RowVector
-}
foldl : (a -> b -> b) -> b -> RowVector a -> b
foldl foldFunction acc (RowVector vector) =
    Vector.foldl foldFunction acc vector


all : (a -> Bool) -> RowVector a -> Bool
all predicate (RowVector vector) =
    Vector.all predicate vector


parseRowVector : Parser.Parser a -> Parser.Parser (RowVector a)
parseRowVector rowVectorElementsParser =
    Parser.succeed RowVector
        |. Parser.keyword "RowVector"
        |. Parser.spaces
        |= Vector.parseVector rowVectorElementsParser
