core.matrix
===========

N-dimensional Matrix / Vector maths API for Clojure.

```clojure
(+ [[1 2]
    [3 4]]
   (* (identity-matrix 2) 3.0))

=> [[4.0 2.0]
    [3.0 7.0]]
```

The objective of `core.matrix` is to provide a common and idiomatic abstraction for matrix maths in Clojure,
independent of underlying implementations. The plan is to become an official Clojure Contrib language extension once the API has matured. Like NumPy, but with more parens and functional goodness..

Key objectives:

 - Provide a clear, standard API / abstraction for matrix and vector maths in Clojure
 - Enable plugable support for different underlying matrix library implementations
 - Provide a general purpose n-dimensional array implementation (NumPy style NDArray)
 - Provide a foundation layer for other projects (e.g. Incanter)
 - Maintain high performance wherever possible

### Status

core.matrix is fully functional and usable in production applications. As well as supporting
the standard Clojure data structures, multiple back end implementations exist that provide optimised
matrix implementations. The most mature implementations are currently:

 - **vectorz-clj** : a fast pure-JVM matrix library for Clojure
 - **Clatrix** : native code matrix library using BLAS

However the API is still *subject to some changes* at present (at least up until release 1.0.0),
so users should be prepared to deal with potential breaking changes in future releases.

[![Build Status](https://travis-ci.org/mikera/matrix-api.png?branch=master)](https://travis-ci.org/mikera/matrix-api)

### Getting Started

Get the latest version from Clojars:

 -  https://clojars.org/net.mikera/core.matrix

For code examples see:

 - https://github.com/mikera/matrix-api/blob/master/src/main/clojure/clojure/core/matrix/examples.clj

 For documentation and further examples see the Wiki:

 - https://github.com/mikera/matrix-api/wiki

### Getting started

Right now the code base is primarily intended for people working on the API + implementation.

You can include the latest released version by adding it as a leiningen or Maven dependency from Clojars, but be aware that this is probably out of data compared to the latest master branch.

 - https://clojars.org/mikera/matrix-api

Some documentation / examples can be found in the Wiki:

 - https://github.com/mikera/matrix-api/wiki

You can also find a protocol implementation summary here:

 - http://mikera.github.io/matrix-api/summary.html

### Contributing

All contributions / ideas welcome!

If you wish to contribute code, please ensure you have a **Clojure Contributors Agreement** signed and on file. For more information see:

 - http://clojure.org/contributing

Discussions related to core.matrix generally take place on the "Numerical Clojure" group:

 - https://groups.google.com/forum/?fromgroups#!forum/numerical-clojure

If you are interested in writing a `core.matrix` implementation, see:

 - https://github.com/mikera/matrix-api/wiki/Implementation-Guide
