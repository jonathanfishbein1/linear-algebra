module Internal.Field exposing
    ( Field
    , complexField
    , realField
    )

import ComplexNumbers


type alias Field a =
    { zero : a
    , one : a
    , multiply : a -> a -> a
    , divide : a -> a -> a
    }


realField : Field Float
realField =
    { zero = 0
    , one = 1
    , multiply = (*)
    , divide = (/)
    }


complexField : Field (ComplexNumbers.ComplexNumberCartesian Float)
complexField =
    { zero = ComplexNumbers.zero
    , one = ComplexNumbers.one
    , multiply = ComplexNumbers.multiply
    , divide = ComplexNumbers.divide
    }
