module LinearAlgebra exposing
    ( Vector(..)
    , add
    )


type Vector a
    = Vector (List a)


add : (a -> a -> a) -> Vector a -> Vector a -> Vector a
add addFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 addFunction vectorOne vectorTwo
