module Internal.ComplexVectorSpace exposing (ComplexVectorSpace, complexAlgebra, realAlgebra)

import ComplexNumbers
import Internal.Field
import Vector


type alias ComplexVectorSpace a =
    { field : Internal.Field.Field a
    , subtractVectors : Vector.Vector a -> Vector.Vector a -> Vector.Vector a
    }


realAlgebra : ComplexVectorSpace Float
realAlgebra =
    { field = Internal.Field.realField
    , subtractVectors = Vector.subtractRealVectors
    }


complexAlgebra : ComplexVectorSpace (ComplexNumbers.ComplexNumberCartesian Float)
complexAlgebra =
    { field = Internal.Field.complexField
    , subtractVectors = Vector.subtractComplexVectors
    }
