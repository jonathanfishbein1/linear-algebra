module Internal.Algebra exposing (Algebra, complexAlgebra, realAlgebra)

import ComplexNumbers
import Vector


type alias Algebra a =
    { zero : a
    , multiply : a -> a -> a
    , divide : a -> a -> a
    , subtractVectors : Vector.Vector a -> Vector.Vector a -> Vector.Vector a
    }


realAlgebra : Algebra Float
realAlgebra =
    { zero = 0
    , multiply = (*)
    , divide = (/)
    , subtractVectors = Vector.subtractRealVectors
    }


complexAlgebra : Algebra (ComplexNumbers.ComplexNumberCartesian Float)
complexAlgebra =
    { zero = ComplexNumbers.zero
    , multiply = ComplexNumbers.multiply
    , divide = ComplexNumbers.divide
    , subtractVectors = Vector.subtractComplexVectors
    }
