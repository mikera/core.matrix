(ns clojure.core.matrix.storage
  "Namespace for array APIs that work with backing storage.

   Storage is defined as:
   - A linear addressable space that can be used as backing for array data
   - May be shared by multiple arrays from the same implementation
   - Is normally mutable, however could be immutable in some cases"
  (:require [clojure.core.matrix.protocols :as mp]))



