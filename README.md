core.matrix
===========

N-dimensional Array / Matrix / Vector API for Clojure.

```clojure
(+ [[1 2] 
    [3 4]] 
   (* (identity-matrix 2) 3.0))
   
=> [[4.0 2.0] 
    [3.0 7.0]]
```

The objective of `core.matrix` is to provide a common and idiomatic abstraction for matrix maths in Clojure, 
independent of underlying implementations. The plan is to develop this 
into an official Clojure Contrib language extension. Like NumPy, but with more parens and functional goodness..

Key objectives:

 - Provide a clear, standard API / abstraction for matrix and vector maths in Clojure
 - Enable plugable support for different underlying matrix library implementations
 - Provide a general purpose n-dimensional array implementation (NumPy style)
 - Provide a foundation layer for other projects (e.g. Incanter)
 - Maintain good performance wherever possible
 
[![Build Status](https://travis-ci.org/mikera/matrix-api.png?branch=master)](https://travis-ci.org/mikera/matrix-api)
 
### Getting Started

Get the latest version from Clojars:

 -  https://clojars.org/net.mikera/core.matrix

For code examples see:

 - https://github.com/mikera/matrix-api/blob/master/src/main/clojure/core/matrix/examples.clj
 
 For documentation and further examples see the Wiki:
 
 - https://github.com/mikera/matrix-api/wiki

### WARNING: Subject to change

This API is still a work in progress and subject to change.

If you build anything on top of it, don't be too surprised if it breaks with the next release.

### Getting started

Right now the code base is primarily intended for people working on the API + implementation.

You can include the latest released version by adding it as a leiningen or Maven dependency from Clojars, but be aware that this is probably out of data compared to the latest master branch.

 - https://clojars.org/mikera/matrix-api
 
Some documentation / examples can be found in the Wiki:
 
 - https://github.com/mikera/matrix-api/wiki


### Contributing

All contributions / ideas welcome!

If you wish to contribute code, please ensure you have a **Clojure Contributors Agreement** signed and on file. For more information see:

 - http://clojure.org/contributing

Discussions related to core.matrix generally take place on the "Numerical Clojure" group:

 - https://groups.google.com/forum/?fromgroups#!forum/numerical-clojure
 
 If you are interested in writing a `core.matrix` implementation, see:
 
 - https://github.com/mikera/matrix-api/wiki/Implementation-Guide
 
