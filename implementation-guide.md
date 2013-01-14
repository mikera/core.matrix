### Implementation guide for core.matrix

This guide is for people who want to create a core.matrix implementation.
i.e. extend core.matrix to work with a new underlying matrix library


### Minimum requirements

#### 1. Create a project for your matrix implementation

This can be any project type (leiningen or Maven).

You want to have a separate project for your core.matrix implementation, which can pull in 
any dependencies it needs. Typically you might import any .jar libraries that are needed
for your underlying matrix implementation.

e.g. if you are wrapping the UJMP Java matrix library, you would add UJMP as a dependency.

#### 2. Add core.matrix as a dependency to your project

This is necessary to get access to the key namespaces you need:

 - **core.matrix.protocols** : contains protocols that must be implemented
 - **core.matrix.implementations** : contains code to register / manage your implementation
 - **core.matrix.compliance-tester**: test code to verify your implementation is correct
 
#### 3. Implement the mandatory protocols

These are documented in `src/main/clojure/core/matrix/protocols.clj`

#### 4. Register your implementation

You should call `core.matrix.implementations/register-implementation` with an instance of your matrix library.

```clojure
(imp/register-implementation my-matrix-instance)
```

You can call this at any point during the loading of your implementation *after* you have
implemented the mandatory protocols for the given instance.

#### 5. Run compliance tests

In your test suite you should call into the compliance tester tool to verify that you have correctly
implemented the core.matrix API.

Code to call the compliance test should look something like this:

```clojure
(deftest compliance-test
  (core.matrix.compliance-tester/compliance-test my-matrix-instance)) 
```

Any error in the compliance test mean that there is a bug somewhere - either your implementation
doesn't conform to the core.matrix API, or we have a bug in the compliance test assumptions :-)

### Optional

#### 1. Implement the optional protocols

You only need the mandatory protocols to have a working core.matrix implementation.

Typically however, you will want to implement some or all of the optional protocols to 
take advantage of special features of your matrix library - using these features may offer
users much better performance than the default core.matrix implementations, which are 
written for flexibility and a generic design rather than for speed.

#### 2. Send a patch for "KNOWN-IMPLEMENTATIONS"

This will potentially enable us to do more clever stuff with implementation metadata 
in the future.

It will also help users discover your library! 




 

