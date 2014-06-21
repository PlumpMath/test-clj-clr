(ns test-clj-clr.utils
  (:require [clojure.reflect :as r]
            [clojure.zip :as zip])
  (:import [clojure.reflect
            Field
            Property
            Method
            Constructor]))

;; probably got the ordering of args wrong.
;; rewrite isn't really suitable for lazy
;; sequence, and should apply to maps and things
;; just as well. Its target should be the first
;; rather than the last argument.


;; obvious missing functions

(defn take-until [pred xs]
  (lazy-seq
    (when-let [s (seq xs)]
      (if (pred (first s))
        [(first s)]
        (cons (first s) (take-until pred (rest s)))))))

;; stupid rewrite thing, pending adequate system =====

;; zippers ----------------------------------------------

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

;; when time comes to extend this to weird datatypes etc
;; give the relevant functions an extra options-map argument.

(defn into-reverses? [x]
  (seq? x))

(defn map-entry? [x]
  (instance? clojure.lang.MapEntry x))

(defn standard-branch? [x] (coll? x))

(defn standard-children [x] (seq x))

(defn standard-make-node [x kids]
  (cond ;; future optimizations ahoy
    (map-entry? x) (vec kids)
    (into-reverses? x) (into (empty x) (reverse kids))
    :else (into (empty x) kids)))

;; map entries still weird, have to be careful
(defn standard-zip [x]
  (zip/zipper
    standard-branch?
    standard-children
    standard-make-node
    x))

(defn zip-move-over-right [loc]
  (if-let [rnxt (zip/right loc)]
    rnxt
    (if-let [up (zip/up loc)]
      (recur up)
      nil)))

(defn loc-depth [loc]
  (count (zip/path loc)))

;; here's a great place for pattern matching. oh well.
;; and lots of other things, this is a hobbled version
;; of levelspec.
(defn level-pred [levelspec]
  (cond
    (number? levelspec)
    (fn [loc]
      (= levelspec (loc-depth loc)))
    
    (sequential? levelspec)
    (let [levelspec' (replace {:infinity Double/PositiveInfinity
                               :inf Double/PositiveInfinity}
                       levelspec)
          [min-lvl max-lvl]
          (case (count levelspec')
            1 (cons 0 levelspec')
            2 levelspec)]
      (fn [loc]
        (<= min-lvl (loc-depth loc) max-lvl)))))

;; this is stupid, we need pattern matching of some sort
;; at least. Is the clojure implementation really incompatible with
;; clojure-clr?
;; a yak shave we don't have time for now. Important, tho.

;; this needs a lot more optimization. should be 
;; able to deal with infinitely deep trees etc
(defn rewrite-leveled [pred f x levelspec]
  (let [lp (level-pred levelspec)]
    (loop [loc (standard-zip x)]
      (if (zip/end? loc)
        (zip/root loc)
        (let [n (zip/node loc)]
          (if (and (lp loc) (pred n))
            (let [loc' (zip/edit loc f)
                  loc'' (zip-move-over-right loc')]
              (if loc''
                (recur loc'')
                (zip/root loc')))
            (recur (zip/next loc))))))))

(defn rewrite
  ;; eager version for now. rewrite* or something for lazy.
  ;; on the other hand for kind of annoying reasons rewrite*
  ;; would be difficult.
  ;; Restricting it to certain levels would be easier, tho
  ([pred f x levelspec]
     (rewrite-leveled pred f x levelspec))
  ([pred f x] 
     (loop [loc (standard-zip x)]
       (if (zip/end? loc)
         (zip/root loc)
         (let [n (zip/node loc)]
           (if (pred n)
             (let [loc' (zip/edit loc f)
                   loc'' (zip-move-over-right loc')]
               (if loc''
                 (recur loc'')
                 (zip/root loc')))
             (recur (zip/next loc))))))))

(declare fixed-point)

(defn rewrite-repeated [pred f x]
  (fixed-point #(rewrite pred f % ) x))

;; fixed point ----------------------------------------

(defn fixed-point-seq [f x]
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

;; scan -------------------------------------------

(defn scan [x]
  (tree-seq standard-branch? standard-children x))

;; fix reflection ----------------------------------

(defn qwik-reflect [x]
  (rewrite-repeated
    (fn [x]
      (or
        (instance? clojure.reflect.Field x)
        (instance? clojure.reflect.Property x)
        (instance? clojure.reflect.Method x)
        (instance? clojure.reflect.Constructor x)))
    #(into {:reflection-type (type %)} (seq %))
    (clojure.reflect/reflect x)))

;; submap ------------------------------------------

(defn submap?
  [sub-map m]
  (every?
    (fn [^clojure.lang.MapEntry e]
      (= e (find m (key e))))
    sub-map))

;; from http://stackoverflow.com/questions/20421405/how-to-check-if-a-map-is-a-subset-of-another-in-clojure
;; (defn submap?
;;   "Checks whether m contains all entries in sub."
;;   [^java.util.Map m ^java.util.Map sub]
;;   (.containsAll (.entrySet m) (.entrySet sub)))


;; protocol introspection, adapted from http://maurits.wordpress.com/2011/01/13/find-which-protocols-are-implemented-by-a-clojure-datatype/

(defn protocol? [maybe-p] ;; awfuly stupid
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
