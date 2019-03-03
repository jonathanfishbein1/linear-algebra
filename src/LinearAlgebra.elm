module LinearAlgebra exposing
    ( Vector(..)
    , add
    , equal
    , map
    , multiply
    )


type Vector a
    = Vector (List a)


add : (a -> a -> a) -> Vector a -> Vector a -> Vector a
add addFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 addFunction vectorOne vectorTwo


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


multiply : (a -> a -> a) -> Vector a -> Vector a -> Vector a
multiply multiplyFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 multiplyFunction vectorOne vectorTwo


equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator (Vector vectorOne) (Vector vectorTwo) =
    List.all ((==) True) <| List.map2 comparator vectorOne vectorTwo
