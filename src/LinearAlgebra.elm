module LinearAlgebra exposing
    ( Vector(..)
    , add
    , equal
    , map
    , multiply
    , sum
    , sumEmpty
    )

import Monoid


type Vector a
    = Vector (List a)


smartMap2 : a -> (a -> a -> a) -> List a -> List a -> List a -> List a
smartMap2 defaultValue f left right acc =
    case ( left, right ) of
        ( l :: ls, r :: rs ) ->
            smartMap2 defaultValue f ls rs (f l r :: acc)

        ( l :: ls, [] ) ->
            smartMap2 defaultValue f ls [] (f l defaultValue :: acc)

        ( [], r :: rs ) ->
            smartMap2 defaultValue f [] rs (f defaultValue r :: acc)

        ( [], [] ) ->
            List.reverse acc


add : a -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
add defaultValue addFunction (Vector listOone) (Vector listTwo) =
    Vector <| smartMap2 defaultValue addFunction listOone listTwo []


map : (a -> b) -> Vector a -> Vector b
map f (Vector vector) =
    Vector <| List.map f vector


multiply : (a -> a -> a) -> Vector a -> Vector a -> Vector a
multiply multiplyFunction (Vector vectorOne) (Vector vectorTwo) =
    Vector <| List.map2 multiplyFunction vectorOne vectorTwo


equal : (a -> a -> Bool) -> Vector a -> Vector a -> Bool
equal comparator (Vector vectorOne) (Vector vectorTwo) =
    List.all ((==) True) <| List.map2 comparator vectorOne vectorTwo


sumEmpty : Vector a
sumEmpty =
    Vector []


{-| Monoidally add two Vectors together
-}
sum : a -> (a -> a -> a) -> Monoid.Monoid (Vector a)
sum defaultValue addF =
    Monoid.monoid sumEmpty (add defaultValue addF)
