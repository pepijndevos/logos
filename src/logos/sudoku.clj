(ns logos.sudoku
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren
        logos.logic))

(def board [(lvar) 7      (lvar) (lvar) (lvar) 8      (lvar) (lvar) 5
            (lvar) (lvar) 8      3      (lvar) 1      (lvar) (lvar) (lvar)
            1      6      3      (lvar) (lvar) 4      8      9      7
            9      8      (lvar) (lvar) (lvar) (lvar) 4      (lvar) (lvar)
            2      (lvar) (lvar) 1      (lvar) 5      (lvar) (lvar) 9
            (lvar) (lvar) 4      (lvar) (lvar) (lvar) (lvar) 3      1
            3      4      6      9      (lvar) (lvar) 1      7      8
            (lvar) (lvar) (lvar) 4      (lvar) 7      9      (lvar) (lvar)
            7      (lvar) (lvar) 8      (lvar) (lvar) (lvar) 6      (lvar)])

(def full-board [5 3 4 6 7 8 9 1 2
                 6 7 2 1 9 5 3 4 8
                 1 9 8 3 4 2 5 6 7
                 8 5 9 7 6 1 4 2 3
                 4 2 6 8 5 3 7 9 1
                 7 1 3 9 2 4 8 5 6
                 9 6 1 5 3 7 2 8 4
                 2 8 7 4 1 9 6 3 5
                 3 4 5 2 8 6 1 7 9])

(def empty-board (repeatedly 81 lvar))

(defn digit-o [d]
  (cond-e
    ((== 1 d))
    ((== 2 d))
    ((== 3 d))
    ((== 4 d))
    ((== 5 d))
    ((== 6 d))
    ((== 7 d))
    ((== 8 d))
    ((== 9 d))))

(defn digit-e [b]
  (cond-e
    ((== b []))
    ((exist [x y]
       (cons-o x y b)
       (digit-o x)
       (digit-e y)))))

(defn board-o [b]
  (all
    (== b (repeatedly 81 lvar))
    (digit-e b)))

(defn permutation-o [s l]
  (cond-e
    ((== s []) (== l []))
    ((exist [x y z]
       (cons-o x y l)
       (rember-o x l z)
       (permutation-o y z)))))

(defn slice [board]
  (let [rows (partition 9 board)
        cols (apply map list rows)
        blockfn (fn [n]
                  (map #(apply concat %)
                    (partition 3
                      (partition 3 9
                        (drop n board)))))
        blocks (concat
                 (blockfn 0)
                 (blockfn 3)
                 (blockfn 6))]
    [rows cols blocks]))

(defmacro sudoku [b]
  (let [board (repeatedly 81 gensym)
        [rows cols blocks] (slice board)]
    (concat
      `(exist [~@board]
              (board-o ~b)
              (== ~b (list ~@board)))
      (map (fn [row]
             `(permutation-o (list ~@row) (range 1 10)))
           rows)
      (map (fn [col]
             `(permutation-o (list ~@col) (range 1 10)))
           cols)
      (map (fn [block]
             `(permutation-o (list ~@block) (range 1 10)))
           blocks))))
