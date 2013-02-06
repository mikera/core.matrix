matrix-api
==========

Experimental N-dimensional Array / Matrix / Vector API for Clojure.

Like NumPy, but with more parentheses.

```clojure
(+ [[1 2] [3 4]] (* (identity-matrix 2) 3.0))
=> [[4.0 2.0] [3.0 7.0]]
```

For code examples see:

 - https://github.com/mikera/matrix-api/blob/master/src/main/clojure/core/matrix/examples.clj

The idea is to provide a common and idiomatic abstraction for matrix maths in Clojure, 
independent of underlying implementations. The plan is to develop this 
into an official Clojure Contrib language extension as `core.matrix`.

Key objectives:

 - Provide a clear, standard API / abstraction for matrix and vector maths in Clojure
 - Enable pluggable support for different underlying matrix library implementations
 - Provide a general purpose n-dimensional array implementation (NumPy style)
 - Provide a foundation layer for other projects (e.g. Incanter)
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

### Writing a matrix implementation

We encourage you to extend the core.matrix protocols to different types of matrices / multi-dimensional
data formats. This is relatively easy and is referred to as a core.matrix "impelmentation". You can write an
implementation for a wide variety of purposes:

 - Java matrix libaries that you want to wrap for use with core.matrix
 - Custom Clojure data structures
 - "array-like" objects that can be viewed as matrices (e.g. bitmap images)
 - Tabular "result-set" objects 
 
Depending on the requirements, you may choose to implement support for some or all core.matrix APIs
 
If you are interested in creating a core.matrix implementation, please read:

 - https://github.com/mikera/matrix-api/blob/master/implementation-guide.md
 
Also please be aware that prior to version 1.0.0, the API is still in flux and you can expect some breaking 
changes.

### Contributing

All contributions / ideas welcome!

If you wish to contribute code, please ensure you have a **Clojure Contributors Agreement** signed and on file. For more information see:
- http://clojure.org/contributing

Also see relevant discussions:
 - https://github.com/tel/clatrix/issues/7
 - https://github.com/tel/clatrix/issues/2
 
