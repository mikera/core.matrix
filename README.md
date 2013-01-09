matrix-api
==========

Experimental N-dimensional Array / Matrix / Vector API for Clojure.

Like NumPy, but for Clojure.

The idea is to provide a common abstraction for matrix maths in Clojure, 
independent of underlying implementations. If it works, there would be an option to 
propose this library to be part of Clojure contrib as `core.matrix`.

Key objectives:

 - Provide a clear, standard API / abstraction for matrix and vector maths in Clojure
 - Enable support for different underlying implementations
 - Provide a base layer for other projects (e.g. Incanter)
 - Maintain good performance where possible

Example implementations we might want to support:

 - Parallel Colt
 - JBLAS / clatrix
 - Vectorz
 - javax.vecmath (part of Java3D)
 - Simple matrices constructed from Clojure vectors
 - EJML
 - and probably many more....
 
 [![Build Status](https://travis-ci.org/mikera/matrix-api.png?branch=master)](https://travis-ci.org/mikera/matrix-api)

### Do not use yet!

This library is experimental / subject to rapid change.

Contributions / ideas welcome however.

see discussions:
 - https://github.com/tel/clatrix/issues/7
 - https://github.com/tel/clatrix/issues/2
 
