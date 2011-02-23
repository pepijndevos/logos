(ns logos.number
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

(defn break-num [n]
  (map #(Integer/parseInt (str %)) (reverse (Integer/toString n 2))))

(defn build-num [l]
  (reduce #(+ %1 (* (first %2) (last %2)))
          0 (map vector (iterate #(bit-shift-left % 1) 1) l)))

(defn full-adder-o [b x y r c]
  (cond-e
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))

(defn pos-o [n]
  (exist [a b]
    (== (lcons a b) n)))

(defn >1-o [n]
  (exist [f fne fnne]
    (== (llist f fne fnne) n)))

(declare gen-adder-o)

(defn adder-o [d n m r]
  (cond-e
    ((== 0 d) (== [] m) (== n r))
    ((== 0 d) (== [] n) (== m r)
     (pos-o m))
    ((== 1 d) (== [] m)
     (adder-o 0 n [1] r))
    ((== 1 d) (== [] n) (pos-o m)
     (adder-o 0 [1] m r))
    ((== [1] n) (== [1] m)
     (exist [a c]
       (== [a c] r)
       (full-adder-o d 1 1 a c)))
    ((== [1] n) (gen-adder-o d n m r))
    ((== [1] m) (>1-o n) (>1-o r)
     (adder-o d [1] n r))
    ((>1-o n) (gen-adder-o d n m r))))

(defn gen-adder-o [d n m r]
    (exist [a b c e x y z]
      (== (lcons a x) n)
      (== (lcons b y) m) (pos-o y)
      (== (lcons c z) r) (pos-o z)
      (all
        (full-adder-o d a b c e)
        (adder-o e x y z))))

(defn +o [n m s]
  (adder-o 0 n m s))

(defn -o [n m s]
  (+o m s n))
