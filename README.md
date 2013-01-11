matrix-api
==========

Experimental N-dimensional Array / Matrix / Vector API for Clojure.

Like NumPy, but with more parentheses.

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
 - Apache Commons maths
 - javax.vecmath (part of Java3D)
 - Simple matrices constructed from Clojure vectors
 - EJML
 - and probably many more....
 
 [![Build Status](https://travis-ci.org/mikera/matrix-api.png?branch=master)](https://travis-ci.org/mikera/matrix-api)

### WARNING: Subject to change

This API is experimental / subject to rapid change.

If you build anything on top of it, don't be surprised if it breaks with the next release. You have been warned.

### Contributing

All contributions / ideas welcome!

If you wish to contribute code, please ensure you have a **Clojure Contributors Agreement** signed and on file. For more information see:
- http://clojure.org/contributing

Also see relevant discussions:
 - https://github.com/tel/clatrix/issues/7
 - https://github.com/tel/clatrix/issues/2
 
