matrix-api
==========

Experimental N-dimensional Array / Matrix / Vector API for Clojure.

Like NumPy, but with more parentheses.

For code examples see:

 - https://github.com/mikera/matrix-api/blob/master/src/main/clojure/core/matrix/examples.clj

The idea is to provide a common abstraction for matrix maths in Clojure, 
independent of underlying implementations. If it works, there would be an option to 
propose this library to be part of Clojure contrib as `core.matrix`.

Key objectives:

 - Provide a clear, standard API / abstraction for matrix and vector maths in Clojure
 - Enable support for different underlying matrix library implementations
 - Provide a base layer for other projects (e.g. Incanter)
 - Maintain good performance wherever possible

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

### Getting started

Right now the code base is primarily intended for people working on the API + implementation.

You can include the latest released version by adding it as a leiningen or Maven dependency from Clojars, but be aware that this is probably out of data compared to the latest master branch.

 - https://clojars.org/mikera/matrix-api


### Contributing

All contributions / ideas welcome!

If you wish to contribute code, please ensure you have a **Clojure Contributors Agreement** signed and on file. For more information see:
- http://clojure.org/contributing

Also see relevant discussions:
 - https://github.com/tel/clatrix/issues/7
 - https://github.com/tel/clatrix/issues/2
 
