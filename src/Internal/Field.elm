module Internal.Field exposing
    ( Field
    , complexField
    , realField
    )

import ComplexNumbers


type alias Field a =
    { zero : a
    , one : a
    , add : a -> a -> a
    , subtract : a -> a -> a
    , multiply : a -> a -> a
    , divide : a -> a -> a
    }


realField : Field Float
realField =
    { zero = 0
    , one = 1
    , add = (+)
    , subtract = (-)
    , multiply = (*)
    , divide = (/)
    }


complexField : Field (ComplexNumbers.ComplexNumberCartesian Float)
complexField =
    { zero = ComplexNumbers.zero
    , one = ComplexNumbers.one
    , add = ComplexNumbers.add
    , subtract = ComplexNumbers.subtract
    , multiply = ComplexNumbers.multiply
    , divide = ComplexNumbers.divide
    }
