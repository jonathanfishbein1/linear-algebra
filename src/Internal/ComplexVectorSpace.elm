module Internal.ComplexVectorSpace exposing (ComplexVectorSpace, complexVectorSpace, realVectorSpace)

import ComplexNumbers
import Internal.Field
import Vector


type alias ComplexVectorSpace a =
    { field : Internal.Field.Field a
    , subtractVectors : Vector.Vector a -> Vector.Vector a -> Vector.Vector a
    }


realVectorSpace : ComplexVectorSpace Float
realVectorSpace =
    { field = Internal.Field.realField
    , subtractVectors = Vector.subtractRealVectors
    }


complexVectorSpace : ComplexVectorSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexVectorSpace =
    { field = Internal.Field.complexField
    , subtractVectors = Vector.subtractComplexVectors
    }
