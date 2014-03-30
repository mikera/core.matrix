(ns clojure.core.matrix.gen-generic
  (:require [clojure.string :as s])
  (:require [clojure.walk :as walk])
  (:require [clojure.java.io :as io])
  (:import [java.io LineNumberReader InputStreamReader PushbackReader]))

(defn get-code "gets the code for the symbol x" [x]
  (when-let [v (resolve x)]
    (let [nspc (str (ns-name (:ns (meta v))))
          filename (str (s/replace (s/replace nspc "." "/" ) "-" "_") ".clj")]
      (when-let [strm (io/input-stream (io/resource filename))]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (:line (meta v)))]
            (.readLine rdr))
          (read (PushbackReader. rdr))
          (if (.available strm)
            (read (PushbackReader. rdr))))))))

(defn get-arities [dfn]
  (let [idx (if (string? (nth dfn 2)) 3 2)
        arities (drop idx dfn)]
    (if (list? (first arities)) arities (list arities))))

(defn add-spec-to-argument-list [arity]
  (list* (vec (cons 'spec (first arity))) (rest arity)))

(defn replace-protocol-function-calls [arity]
  (walk/postwalk #(if (and (list? %) (symbol? (first %))
                           (.startsWith ^String (str (first %)) "mp/"))
                    (list* (symbol (str "gmp/generic-" (name (first %))))
                           (concat (rest %) ['spec]))
                    %) arity))

(defn to-defn [code arities]
  (let [idx (if (string? (nth code 2)) 3 2)]
    (list* (concat (take idx code) arities))))

(def num-to-gen
  {1.0 '(:one spec)
   1 '(:one spec)
   0 '(:zero spec)
   0.0 '(:zero spec)
   '* '(:mul spec)
   '+ '(:add spec)
   '- '(:sub spec)
   '/ '(:div spec)
   'number? '(:scalar? spec)})

(defn replace-with-spec-content [arity]
  (let [to-replace (set (filter (set (keys num-to-gen)) (flatten arity)))
        gensym-bindings (mapv (fn [s] [(gensym) s]) to-replace)
        gensym-map (into {} (map (comp vec reverse) gensym-bindings))
        ]
    `(~(first arity)
      (let ~(vec (mapcat identity gensym-bindings))
        ~@(walk/postwalk-replace gensym-map (rest arity))))))

(defn generify-code [code]
  (->> code
      get-arities
      (map add-spec-to-argument-list)
      (map replace-protocol-function-calls)
      (map replace-with-spec-content)
      (to-defn code)
      ))

(defn create-generic-code [list-of-functions]
  (let [func (first list-of-functions)
        code (get-code func)]
    (map (comp generify-code  get-code) list-of-functions)))
