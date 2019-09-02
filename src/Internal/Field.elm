module Internal.Field exposing
    ( Field
    , complexField
    , realField
    )

import ComplexNumbers


type alias Field a =
    { zero : a
    , multiply : a -> a -> a
    , divide : a -> a -> a
    }


realField : Field Float
realField =
    { zero = 0
    , multiply = (*)
    , divide = (/)
    }


complexField : Field (ComplexNumbers.ComplexNumberCartesian Float)
complexField =
    { zero = ComplexNumbers.zero
    , multiply = ComplexNumbers.multiply
    , divide = ComplexNumbers.divide
    }
