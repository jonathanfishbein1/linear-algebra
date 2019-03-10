# ComplexeNumbers

## Summary

A module for vector and matrix operations

There exist other elm linear algebra packages so I'll wont explain the fundamental concetps and instead describe how this package
differs from the others.  This package is a native elm pacakge for linear algebra as opposed to delegating tasks out to Javascript.  Additionally,
 the vector and matrix types here have a parameterized type so instead of being just restricted to Ints, Floats or number types vectors and matrixes can hold
 anytime of value.  I have focused the package on vectors and matrices holding complex numbers although you can put whatever you'd like in them.


Finally I have attempted to deisgn the package in an idiomatic Haskell style and I'm still working to achieve that.  With both vector and matrix having 
parameterized types they both can be made instances of Functor, Applicative and Monad. Additionally, I am working on making them instances of Maonoid.
