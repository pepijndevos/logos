(ns logos.number
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

(defn overflow
  "AND"
  [base x y]
  (if (< (+ x y) base)
    0
    1))

(defn bound
  "XOR"
  [base x y]
  (rem (+ x y) base))

;; OR = max

(defn build-num-list [base n]
  (map #(Integer/parseInt (str %)) (reverse (Integer/toString n base))))

(defn build-num [base l]
  (reduce #(+ %1 (* (first %2) (last %2)))
          0 (map vector (iterate #(* base %) 1) l)))

(defmacro addfn [base]
  (let [bg (gensym)
        xg (gensym)
        yg (gensym)
        rg (gensym)
        cg (gensym)
        conditions (for [b [0 1]
                         x (range base)
                         y (range base)
                         :let [half (bound base x y)
                               r (bound base half b)
                               c (max (overflow base x y)
                                      (overflow base half b))]]
                         `((== ~b ~bg) (== ~x ~xg) (== ~y ~yg) (== ~r ~rg) (== ~c ~cg)))]
    (concat `(fn [~bg ~xg ~yg ~rg ~cg])
            (list (cons `cond-e conditions)))))

(def ^:dynamic *full-adder-o* (addfn 2))

(defmacro with-adder [base & exp]
  `(binding [*full-adder-o* (addfn ~base)] ~@exp))

(defn pos-o [n]
  (exist [a b]
    (== (lcons a b) n)))

(defn >b-o [n]
  (exist [f fne fnne]
    (== (llist f fne fnne) n)))

(declare gen-adder-o)

(defn adder-o [d n m r]
  (exist [e f]
    (cond-e
      ((== 0 d) (== [] m) (== n r))
      ((== 0 d) (== [] n) (== m r)
       (pos-o m))
      ((== 1 d) (== [] m)
       (adder-o 0 n [1] r))
      ((== 1 d) (== [] n) (pos-o m)
       (adder-o 0 [1] m r))
      ((== [e] n) (== [f] m)
       (exist [a c]
         (== [a c] r)
         (*full-adder-o* d e f a c)))
      ((== [e] n) (gen-adder-o d n m r))
      ((== [e] m) (>b-o n) (>b-o r)
       (adder-o d [e] n r))
      ((>b-o n) (gen-adder-o d n m r)))))

(defn gen-adder-o [d n m r]
    (exist [a b c e x y z]
      (== (lcons a x) n)
      (== (lcons b y) m) (pos-o y)
      (== (lcons c z) r) (pos-o z)
      (all
        (*full-adder-o* d a b c e)
        (adder-o e x y z))))

(defn +o [n m s]
  (adder-o 0 n m s))

(defn -o [n m s]
  (+o m s n))
