(ns logos.sudoku
  (:refer-clojure :exclude [reify == inc])
  (:use [logos
           minikanren
           logic
           nonrel]))

(def board [(lvar) 7      (lvar) (lvar) (lvar) 8      (lvar) (lvar) 5
            (lvar) (lvar) 8      3      (lvar) 1      (lvar) (lvar) (lvar)
            1      6      3      (lvar) (lvar) 4      8      9      7
            9      8      (lvar) (lvar) (lvar) (lvar) 4      (lvar) (lvar)
            2      (lvar) (lvar) 1      (lvar) 5      (lvar) (lvar) 9
            (lvar) (lvar) 4      (lvar) (lvar) (lvar) (lvar) 3      1
            3      4      6      9      (lvar) (lvar) 1      7      8
            (lvar) (lvar) (lvar) 4      (lvar) 7      9      (lvar) (lvar)
            7      (lvar) (lvar) 8      (lvar) (lvar) (lvar) 6      (lvar)])

(def bad-board [9      4      2      5      3      7      6      1      8
                (lvar) 7      (lvar) (lvar) 8      6      9      4      3
                (lvar) 3      8      (lvar) (lvar) 1      2      7      5
                3      (lvar) 7      6      1      (lvar) 4      (lvar) (lvar)
                4      (lvar) (lvar) (lvar) 5      (lvar) 7      3      6
                (lvar) 2      6      3      (lvar) 4      1      (lvar) 9
                8      6      (lvar) (lvar) (lvar) 5      8      2      (lvar)
                (lvar) (lvar) 9      4      2      8      3      (lvar) 7
                2      8      (lvar) 7      (lvar) 3      5      9      1])

(def full-board [5 3 4 6 7 8 9 1 2
                 6 7 2 1 9 5 3 4 8
                 1 9 8 3 4 2 5 6 7
                 8 5 9 7 6 1 4 2 3
                 4 2 6 8 5 3 7 9 1
                 7 1 3 9 2 4 8 5 6
                 9 6 1 5 3 7 2 8 4
                 2 8 7 4 1 9 6 3 5
                 3 4 5 2 8 6 1 7 9])

(def almost-board [5 3 (lvar) 6 7 8 9 1 (lvar)
                   6 7 (lvar) 1 9 5 3 4 (lvar)
                   1 9 (lvar) 3 4 2 5 6 (lvar)
                   8 5 (lvar) 7 6 1 4 2 (lvar)
                   4 2 (lvar) 8 5 3 7 9 (lvar)
                   7 1 (lvar) 9 2 4 8 5 (lvar)
                   9 6 (lvar) 5 3 7 2 8 (lvar)
                   2 8 (lvar) 4 1 9 6 3 (lvar)
                   3 4 (lvar) 2 8 6 1 7 (lvar)])

(def empty-board (repeatedly 81 lvar))

(defn disj-o [x l out]
  (cond-e ;a
   ((cons-o x out l))
   ((exist [f r i]
           (cons-o f r l)
           (cons-o f i out)
           (disj-o x r i)))))

(defn permutation-o [s l]
  (cond-e
    ((== s []) (== l []))
    ((exist [a b x y z]
       (cons-o a b s)
       (cons-o x y l)
       (disj-o x s z)
       (permutation-o z y)))))

(defn blocks [board]
  (mapcat (fn [n]
            (map #(apply concat %)
              (partition 3
                (partition 3 9
                  (drop n board)))))
          [0 3 6]))

(defn slice [board]
  (let [rows (partition 9 board)
        cols (apply map list rows)
        blocks (blocks board)]
    [rows cols blocks]))

(defmacro sudoku [b]
  (let [board (repeatedly 81 gensym)
        [rows cols blocks] (slice board)
        permfn (fn [row]
                 `(permutation-o (list ~@row) (range 1 10)))]
    (concat
      `(exist [~@board]
              (== ~b (list ~@board)))
      (interleave
        (map permfn rows)
        (map permfn cols)
        (map permfn blocks)))))
