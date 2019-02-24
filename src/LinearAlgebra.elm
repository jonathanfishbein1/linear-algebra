module LinearAlgebra exposing (Vector2(..))


type Vector a
    = Vector (List a)


add : Vector a -> Vector a -> Vector a
add vectorOne vectorTwo =
    List.map2 (+) vectorOne vectorTwo
