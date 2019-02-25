module LinearAlgebra exposing
    ( Vector(..)
    , add
    , zero
    )


type Vector a
    = Vector (List a)


zero : Int -> a -> Vector a
zero n a =
    Vector <| List.repeat n a


add : (a -> a -> a) -> Vector a -> Vector a -> Vector a
add addFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 addFunction vectorOne vectorTwo


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector
