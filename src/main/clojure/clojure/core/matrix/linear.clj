(ns clojure.core.matrix.linear
  "Namespace for core.matrix linear algebra API.

   These function complement the main core.matrix API with specialised functions for linear
   algebra operations."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :refer :all]))


(defn norm
  "Computes the norm of a matrix or vector.

   By default calculates 2-norm for vectors and Frobenius 2-norm for matrices. The optional p argument specifies use of the p-norm instead.

   Special cases of p argument:
   Double/POSITIVE_INFINITY - Infinity norm

   Intended usage: (let [n (norm v 1)] ....)
                   (let [n (norm v Double/POSITIVE_INFINITY)] ....)
                   (let [n (norm v)] ....)"
  ([m] (norm m 2))
  ([m p] (mp/norm m p)))

(defn rank
  "Computes the rank of a matrix, i.e. the number of linearly independent rows.

   Intended usage: (let [r (rank m)] ....)"
  ([m] (mp/rank m)))

(defn qr
  "Computes QR decomposition of a full rank matrix.
   Returns a map containing matrices of an input matrix type with the keys [:Q :R] such that:
        M = Q.R

   Where:
    - Q is an orthogonal matrix
    - R is an upper triangular matrix (= right triangular matrix)
   If :return parameter is specified in options map, it returns only specified keys.
   If :compact parameter is specified in options map, compact versions of matrices are returned.

   Returns nil if decomposition is impossible.

   Intended usage: (let [{:keys [Q R]} (qr M)] ....)
                   (let [{:keys [R]} (qr M {:return [:R]})] ....)"

  ([m {:keys [return compact]
       :or {return [:Q :R]
            compact false}}]
     (mp/qr m {:return return
               :compact compact}))
  ([m]
     (mp/qr m {:return [:Q :R]})))

(defn cholesky
  "Computes the Cholesky decomposition of a hermitian, positive definite matrix.
   Returns a map containing two matrices with the keys [:L :L*] such that

   Such that:
     M = L.L*

   Where
     - M must be a hermitian, positive definite matrix
     - L is a lower triangular matrix
     - L* is the conjugate transpose of L

   If :return parameter is specified in options map, it returns only specified keys.

   Intended usage: (let [{:keys [L L*]} (cholesky M)] ....)
                   (let [{:keys [L*]} (cholesky M {:return [:L*]})] ....)"
  ([m options] (mp/cholesky m options))
  ([m] (cholesky m {:return [:L :L*]})))

(defn lu
  "Computes the LU(P) decomposition of a matrix with partial row pivoting.
   Returns a map containing the keys [:L :U :P], such that:
     P.A = L.U

   Where
     - L is a lower triangular matrix
     - U is an upper triangular matrix
     - P is a permutation matrix

   If :return parameter is specified in options map, it returns only specified keys.

   Intended usage: (let [{:keys [L U P]} (lu A)] ....)
                   (let [{:keys [L U]} (lu M {:return [:L :U]})] ....)"

  ([m options] (mp/lu m options))
  ([m] (lu m {:return [:L :U :P]})))

(defn svd
  "Computes the Singular Value decomposition of a matrix.
   Returns a map containing the keys [:U :S :V*] such that:
     M = U.S.V*

   Where
     - U is an unitary matrix
     - S is a diagonal matrix whose elements are the singular values of the original matrix
     - V* is an unitary matrix

   If :return parameter is specified in options map, it returns only specified keys.

   Intended usage: (let [{:keys [U S V*]} (svd M)] ....)
                   (let [{:keys [S]} (svd M {:return [:S]})] ....)"

  ([m options] (mp/svd m options))
  ([m] (svd m {:return [:U :S :V*]})))

(defn eigen
  "Computes the Eigen decomposition of a diagonalisable matrix.
   Returns a map containing matrices for each of the the keys [:Q :A] such that:

      M l= Q.A.Q<sup>-1</sup>

   Where:
     - Q is a matrix where each column is the ith normalised eigenvector of M
     - A is a diagonal matrix whose diagonal elements are the eigenvalues.
     - Q<sup>-1</sup> is the inverse of Q

   If :return parameter is specified in options map, it returns only specified keys.

   Intended usage: (let [{:keys [Q A]} (eigen M)] ....)
                   (let [{:keys [A]} (eigen M {:return [:A]})] ....)"
  ([m options] (mp/eigen m options))
  ([m] (eigen m {:return [:Q :A]})))

(defn solve
  "Solves a linear matrix equation, or system of linear scalar equations, i.e. finds the
   value X such that:

     A.X = B

   Where:
     - A is a square matrix containing the coefficients of the linear system
     - B is a vector containing the right-hand side of the linear system.
   If B is missing, it is taken as an identity matrix and returns inverse of A

   Intended usage: (let [X (solve A B)] ....)"
  ([a b] (mp/solve a b))
  ([a] (mp/inverse a)))

(defn least-squares
  "Computes least-squares solution to a linear matrix equation.

   Intended usage: (let [X (least-squares A B)] ....)"
  ([a b] (mp/least-squares a b)))
