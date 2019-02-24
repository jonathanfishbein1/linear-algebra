module LinearAlgebra exposing
    ( Vector(..)
    , add
    )


type Vector a
    = Vector (List a)


add : Vector number -> Vector number -> Vector number
add (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 (+) vectorOne vectorTwo
