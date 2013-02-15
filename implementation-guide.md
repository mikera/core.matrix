### Implementation guide for clojure.core.matrix

This guide is for people who want to create a clojure.core.matrix implementation.
i.e. extend clojure.core.matrix to work with a new underlying matrix library


### Minimum requirements

#### 1. Create a project for your matrix implementation

This can be any project type (leiningen or Maven).

You want to have a separate project for your clojure.core.matrix implementation, which can pull in 
any dependencies it needs. Typically you might import any .jar libraries that are needed
for your underlying matrix implementation.

e.g. if you are wrapping the UJMP Java matrix library, you would add UJMP as a dependency.

#### 2. Add clojure.core.matrix as a dependency to your project

This is necessary to get access to the key namespaces you need:

 - **clojure.core.matrix** : contains the user-facing API
 - **clojure.core.matrix.protocols** : contains protocols that must be implemented
 - **clojure.core.matrix.implementations** : contains code to register / manage your implementation
 - **clojure.core.matrix.compliance-tester**: test code to verify your implementation is correct
 
#### 3. Implement the mandatory protocols

These are documented in `src/main/clojure/core/matrix/protocols.clj`

#### 4. Register your implementation

You should call `clojure.core.matrix.implementations/register-implementation` with an instance of your matrix library.

```clojure
(imp/register-implementation my-matrix-instance)
```

You can call this at any point during the loading of your implementation *after* you have
implemented the mandatory protocols for the given instance.

#### 5. Run compliance tests

In your test suite you should call into the compliance tester tool to verify that you have correctly
implemented the clojure.core.matrix API.

Code to call the compliance test should look something like this:

```clojure
(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test my-matrix-instance)) 
```

Any error in the compliance test mean that there is a bug somewhere - either your implementation
doesn't conform to the clojure.core.matrix API, or we have a bug in the compliance test assumptions :-)

### Optional

#### 1. Implement the optional protocols

You only need the mandatory protocols to have a working clojure.core.matrix implementation.

Typically however, you will want to implement some or all of the optional protocols to 
take advantage of special features of your matrix library - using these features may offer
users much better performance than the default clojure.core.matrix implementations, which are 
written for flexibility and a generic design rather than for speed.

#### 2. Send a patch for "KNOWN-IMPLEMENTATIONS"

This will potentially enable us to do more clever stuff with implementation metadata 
in the future.

It will also help users discover your library! 


### Implementation Guidance

#### Handling interop with other implementations

When control enters your implementation via one of the protocol functions, you don't know much
about the type of the other arguments. This is particularly important when the other argument
may be a matrix from a different clojure.core.matrix implementation.

Such operations in clojure.core.matrix are left to the discretion of the matrix implementation. You are required to 
either:

- Perform the operation and return a valid result.
- Throw an exception.

Here are your options, in rough order of preference:

 - **Coerce the other matrix to your format** - via calling `(coerce your-matrix other-matrix)`. This
 may be an expensive operation (likely to require constructing a whole new matrix in your format) but 
 should work effectively as a general approach
 - **Defer to a generic mutimethod** - clojure.core.matrix provides some generic multimethods that should perform 
 the necessary operations, e.g. `clojure.core.matrix.multimethods/mul`. Generic implementations are likely to be slow but correct. 
 In some cases the generic method may be able to exploit optimisations, e.g. multiplication of two diagonal matrices from different implementations.
 - **Explicitly recognise and work with the other implementation** - this requires hard-coding and is a lot of work.
 Probably not recommended except for special cases where you really need to work well with another specific implementation
 - **Give a warning** - useful in combination with one of the other methods if your library is focused on performance,
 to let the user know that they are taking a performance hit while still attempting to produce a correct result.
 - **Throw an exception** - This is not ideal, but may be acceptable if you are willing
 to label your implementation clearly as incomplete / not compatible with other implementations.



 

