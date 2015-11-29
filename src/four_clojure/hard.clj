(ns four-clojure.hard)

(defn problem-fifty-three
  "Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers.
  If two sub-sequences have the same length, use the one that occurs first.
  An increasing sub-sequence must have a length of 2 or greater to qualify."
  []
  (let [longest-inc-seq (fn [coll]

                          (let [into-pairs (partial partition 2 1)
                                is-increasing (partial apply <)
                                taking-longest #(if (> (count %2) (count %1)) %2 %1)
                                each-first (partial map first)
                                last-last (comp last last)
                                coalesce-pairs #(conj (into [] (each-first %)) (last-last %))
                                empty-if-not-increasing #(if (is-increasing %) % [])]

                            (->> (into-pairs coll)
                                 (partition-by is-increasing)
                                 (reduce taking-longest)
                                 (coalesce-pairs)
                                 (empty-if-not-increasing))))]

    (= (longest-inc-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
    (= (longest-inc-seq [5 6 1 3 2 7]) [5 6])
    (= (longest-inc-seq [2 3 3 4 5]) [3 4 5])
    (= (longest-inc-seq [7 6 5 4]) [])))

(defn problem-seventy-three
  "A tic-tac-toe board is represented by a two dimensional vector.
  X is represented by :x, O is represented by :o, and empty is represented by :e.
  A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row.
  Write a function which analyzes a tic-tac-toe board and returns :x if X has won,
  :o if O has won, and nil if neither player has won."
  []
  (let [winner (fn [board]
                 (let [row   (fn [n b] (b n))
                       col   (fn [n b] (into [] (map #(% n) b)))
                       ldiag (fn [b]
                               (->> (reduce
                                      (fn [[ld i] r]
                                        (vector
                                          (conj ld (r i))
                                          (inc i)))
                                      [[] 0] b)
                                    (first)))
                       rdiag (fn [b]
                               (->> (reduce
                                      (fn [[rd i] r]
                                        (vector
                                          (conj rd (r i))
                                          (dec i)))
                                      [[] 2] b)
                                    (first)))
                       xs    (fn [s] (map #(if (= :x %) 1 0) s))
                       os    (fn [s] (map #(if (= :o %) 1 0) s))
                       won   (fn [p]
                               (->>
                                 (map
                                   (fn [f]
                                     (->>
                                       (f board)
                                       (p)
                                       (apply +)))
                                   [(partial row 0)
                                    (partial row 1)
                                    (partial row 2)
                                    (partial col 0)
                                    (partial col 1)
                                    (partial col 2)
                                    ldiag
                                    rdiag])
                                 (filter #(= 3 %))
                                 (seq)))]
                   (cond
                     (won xs) :x
                     (won os) :o
                     :else nil)))]
    (= nil (winner [[:e :e :e]
                    [:e :e :e]
                    [:e :e :e]]))
    (= :x (winner [[:x :e :o]
                   [:x :e :e]
                   [:x :e :o]]))
    (= :o (winner [[:e :x :e]
                   [:o :o :o]
                   [:x :e :x]]))
    (= nil (winner [[:x :e :o]
                    [:x :x :e]
                    [:o :x :o]]))
    (= :x (winner [[:x :e :e]
                   [:o :x :e]
                   [:o :e :x]]))
    (= :o (winner [[:x :e :o]
                   [:x :o :e]
                   [:o :e :x]]))
    (= nil (winner [[:x :o :x]
                    [:x :o :x]
                    [:o :x :o]]))))

(defn problem-seventy-nine
  "Write a function which calculates the sum of the minimal path through a triangle.
  The triangle is represented as a collection of vectors.
  The path should start at the top of the triangle and move to an adjacent number
  on the next row until the bottom of the triangle is reached."
  []
  (let [min-path-sum (fn [triangle]
                       (->> triangle
                            (reverse)
                            (reduce (fn [bot top]
                                      (->> (partition 2 1 bot)
                                           (map #(apply min %))
                                           (map vector top)
                                           (map #(apply + %))
                                           (min))))
                            (first)))]
    (= 7 (min-path-sum '([1]
                         [2 4]
                         [5 1 4]
                         [2 3 4 5])))

    (= 20 (min-path-sum '([3]
                          [2 4]
                          [1 9 3]
                          [9 9 2 4]
                          [4 6 6 7 8]
                          [5 7 3 5 1 4])))))
