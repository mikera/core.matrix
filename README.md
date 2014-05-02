core.matrix
===========

core.matrix provides array programming as a language extension for Clojure, with a focus on numerical computing.

The central objective of `core.matrix` is to make matrix and array programming
idiomatic, productive, elegant and fast in the Clojure environment.

```clojure
(+ [[1 2]
    [3 4]]
   (* (identity-matrix 2) 3.0))

=> [[4.0 2.0]
    [3.0 7.0]]

;; Note: nested Clojure vectors can be used as matrices
```

Key goals of `core.matrix`:

 - Provide a clear, standard API / abstraction for **matrix and array programming** in Clojure
 - Enable **pluggable** support for different underlying matrix library implementations
 - Provide a general purpose **n-dimensional array** implementation (NDArray)
 - Provide a **foundation** library for other projects (e.g. Incanter)
 - Enable **high performance** numerical computing

### Getting Started

To use `core.matrix`, you can get the latest released version from Clojars:

 - https://clojars.org/net.mikera/core.matrix

For code examples see:

 - https://github.com/mikera/core.matrix/blob/master/src/main/clojure/clojure/core/matrix/examples.clj

### Documentation

For a general introduction, the slide and video from the 2013 Clojure Conj talk are available here:

 - http://www.slideshare.net/mikeranderson/2013-1114-enter-thematrix
 - http://www.youtube.com/watch?v=_h9TLJtjSJo

For `core.matrix` documentation and further examples see the Wiki:

 - https://github.com/mikera/core.matrix/wiki

### Status

`core.matrix` is fully functional and usable in production applications. As well as supporting
the standard Clojure data structures, multiple back end implementations exist that provide optimised
matrix implementations. The most mature implementations are currently:

 - [**vectorz-clj**](https://github.com/mikera/vectorz-clj) : a fast pure-JVM matrix library for Clojure
 - [**Clatrix**](https://github.com/tel/clatrix) : native code matrix library using BLAS
 - **NDArray** : a general purpose pure Clojure N-dimensional array implementation, included as part of `core.matrix` itself

However the API is still *subject to some changes* at present (at least up until release 1.0.0),
so users should be prepared to deal with potential breaking changes when updating to future releases.

The plan is to become an official Clojure Contrib language extension once the API has been well tested.

[![Build Status](https://travis-ci.org/mikera/core.matrix.png?branch=master)](https://travis-ci.org/mikera/core.matrix)

### Contributing

All contributions / ideas welcome!

There are a number of proposed enhancements listed as GitHub issues: these are a good place to start if you wish to contribute
to core.matrix:

 - https://github.com/mikera/core.matrix/issues?state=open

If you wish to contribute code, please ensure you have a **Clojure Contributors Agreement** signed and on file. For more information see:

 - http://clojure.org/contributing

Discussions related to core.matrix generally take place on the "Numerical Clojure" group:

 - https://groups.google.com/forum/?fromgroups#!forum/numerical-clojure

If you are interested in writing a `core.matrix` implementation, see:

 - https://github.com/mikera/core.matrix/wiki/Implementation-Guide

You can also find a protocol implementation summary here:

 - http://mikera.github.io/core.matrix/summary.html

### Thanks

YourKit is kindly supporting this open source project with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:
<a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
<a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.
