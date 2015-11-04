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
