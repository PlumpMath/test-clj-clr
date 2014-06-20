(ns test-clj-clr.utils
  (:require [clojure.reflect :as r]
            [clojure.zip :as zip])
  (:import [clojure.reflect
            Field
            Property
            Method
            Constructor]))


;; obvious missing function

(defn take-until [pred xs]
  (lazy-seq
    (when-let [s (seq xs)]
      (if (pred (first s))
        [(first s)]
        (cons (first s) (take-until pred (rest s)))))))

;; stupid rewrite thing, pending adequate system

;; (defn zip-branch-dispatch [x]
;;   ())

;; (defmulti zip-branch? #'zip-branch-dispatch)

;; (defn zip-children-dispatch [x]
;;   (type x))

;; (defmulti zip-children #'zip-children-dispatch)

;; (defn zip-make-node-dispatch  )

;; (defmulti zip-make-node #'zip-make-node-dispatch)
;; bla bla bla

;; fuck it, let's write one that at least works for clojure data

(defn into-reverses? [x]
  (seq? x))

(defn map-entry? [x]
  (instance? clojure.lang.MapEntry x))

;; map entries still weird, have to be careful
(defn standard-zip [x]
  (zip/zipper
    (fn [x] (coll? x))
    (fn [x] (seq x))
    (fn [x kids] ;; future optimizations ahoy
      (cond
        (map-entry? x) (vec kids)
        (into-reverses? x) (into (empty x) (reverse kids))
        :else (into (empty x) kids)))
    x))

(defn zip-move-over [loc]
  ())

(defn qwik-rewrite [x, f, pred]
  (->> x
    standard-zip
    (iterate
      (fn [loc]
        (let [n (zip/node loc)]
          (if (pred node)
            (zip-move-over
              (zip/replace loc (f node)))
            (zip/next loc)))))
    (filter zip/end?)
    first
    zip/root))

;; fixedpoint 

(defn to-fixed-point [f x]
  ((fn step [x]
     (cons x
       (lazy-seq
         (let [x' (f x)]
           (when (not= x x')
             (step x'))))))
   x))

(defn fixed-point [f x]
 (loop [x x, x' (f x)]
   (if (= x x')
     x
     (recur x' (f x)))))

;; fix reflection

(defn qwik-reflect [x]
  (qwik-rewrite (clojure.reflect/reflect x)
    ))

;; protocol introspection, adapted from http://maurits.wordpress.com/2011/01/13/find-which-protocols-are-implemented-by-a-clojure-datatype/

(defn protocol? [maybe-p]
  (boolean (:on-interface maybe-p)))

(defn all-protocols 
  ([] (all-protocols *ns*))
  ([ns]
     (filter #(protocol? @(val %))
       (ns-publics ns))))

(defn implemented-protocols
  ([x] (implemented-protocols x *ns*))
  ([x ns]
     (filter #(satisfies? @(val %) x) (all-protocols ns))))

;; macros straight from 1.5

(defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  {:added "1.5"}
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) forms)]
     ~name))


(defmacro cond->
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
  through each form for which the corresponding test
  expression is true. Note that, unlike cond branching, cond-> threading does
  not short circuit after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if ~test (-> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro cond->>
  "Takes an expression and a set of test/form pairs. Threads expr (via ->>)
  through each form for which the corresponding test expression
  is true.  Note that, unlike cond branching, cond->> threading does not short circuit
  after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if ~test (->> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

(defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  {:added "1.5"}
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) forms)]
     ~name))

(defmacro some->
  "When expr is not nil, threads it into the first form (via ->),
  and when that result is not nil, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep forms))]
       ~g)))

(defmacro some->>
  "When expr is not nil, threads it into the first form (via ->>),
  and when that result is not nil, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(if (nil? ~g) nil (->> ~g ~step)))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep forms))]
       ~g)))
