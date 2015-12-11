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

(defn problem-eighty-two
  "A word chain consists of a set of words ordered so that each word differs by only one letter
  from the words directly before and after it. The one letter difference can be either an insertion, a deletion,
  or a substitution. Here is an example word chain:

  cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog

  Write a function which takes a sequence of words, and returns true if they can be arranged
  into one continous word chain, and false if they cannot."
  []
  (let [chainable? (fn [word-set]
                      (letfn [(permute [arr]
                                (let [conc (fn [x y] (vec (concat x (if (vector? y) y (vector y)))))
                                      except-idx (fn [idx coll] (vec (concat (take idx coll) (nthrest coll (inc idx)))))]
                                  (reduce
                                    (fn [a b] (conc (vec a) (vec b)))
                                    (map-indexed
                                      (fn [i v]
                                        (let [prefix (vector v)
                                              remainder (except-idx i arr)]
                                          (map
                                            (partial conc prefix)
                                            (if (> (count remainder) 1)
                                              (permute remainder)
                                              remainder))))
                                      arr))))

                              (edit-distance [w1 w2]
                                (letfn [(cell-value [same-char? prev-row cur-row col-idx]
                                          (min (inc (nth prev-row col-idx))
                                               (inc (last cur-row))
                                               (+ (nth prev-row (dec col-idx)) (if same-char? 0 1))))]
                                  (loop [row-idx  1
                                         max-rows (inc (count w2))
                                         prev-row (range (inc (count w1)))]
                                    (if (= row-idx max-rows)
                                      (last prev-row)
                                      (let [ch2           (nth w2 (dec row-idx))
                                            next-prev-row (reduce (fn [cur-row i]
                                                                    (let [same-char? (= (nth w1 (dec i)) ch2)]
                                                                      (conj cur-row (cell-value same-char?
                                                                                                prev-row
                                                                                                cur-row
                                                                                                i))))
                                                                  [row-idx] (range 1 (count prev-row)))]
                                        (recur (inc row-idx) max-rows next-prev-row))))))]

                        (->> (permute (into [] word-set))
                             (filter (fn [permutation]
                                       (every? (partial = 1)
                                               (map (partial apply edit-distance)
                                                    (partition 2 1 permutation)))))
                             (count)
                             (< 0))))]
    (= true  (chainable? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
    (= false (chainable? #{"cot" "hot" "bat" "fat"}))
    (= false (chainable? #{"to" "top" "stop" "tops" "toss"}))
    (= true  (chainable? #{"spout" "do" "pot" "pout" "spot" "dot"}))
    (= true  (chainable? #{"share" "hares" "shares" "hare" "are"}))
    (= false (chainable? #{"share" "hares" "hare" "are"}))))

(defn problem-eighty-four
  "Write a function which generates the transitive closure of a binary relation.
  The relation will be represented as a set of 2 item vectors."
  []
  (let [transitive-closure (fn [pair-set]
                             (let [pair-map (into {} pair-set)]
                               (->> (map
                                      (fn [pair]
                                        (take-while
                                          (fn [[_ v]] (not (nil? v)))
                                          (iterate
                                            (fn [[k v]]
                                              (vector k (pair-map v)))
                                            pair)))
                                      pair-map)
                                    (reduce concat)
                                    (set))))]

    (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
      (= (transitive-closure divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
    (let [more-legs
          #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
      (= (transitive-closure more-legs)
         #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
           ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
    (let [progeny
          #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
      (= (transitive-closure progeny)
         #{["father" "son"] ["father" "grandson"]
           ["uncle" "cousin"] ["son" "grandson"]}))))

(defn problem-eighty-nine
  "Starting with a graph you must write a function that returns true
  if it is possible to make a tour of the graph in which every edge is visited exactly once.

  The graph is represented by a vector of tuples, where each tuple represents a single edge.

  The rules are:

  - You can start at any node.
  - You must visit each edge exactly once.
  - All edges are undirected."
  []
  (let [tourable? (fn tourable? [nodes]
                    (let [node-has? (fn [node edge] (some #(= edge %) node))
                          except-idx (fn [idx coll] (vec (concat (take idx coll) (nthrest coll (inc idx)))))]
                      (cond
                        (= (count nodes) 0) false
                        (= (count nodes) 1) true
                        :else (every?
                                true?
                                (map-indexed
                                  (fn all-edges-connected-and-even-degree [i node]
                                    (let [a (first node)
                                          b (second node)
                                          degree-of-edges (count (filter #(or
                                                                           (node-has? % a)
                                                                           (node-has? % b))
                                                                         (except-idx i nodes)))]
                                      (and
                                        (> degree-of-edges 0)
                                        (even? degree-of-edges))))
                                  nodes)))))]
    (= true  (tourable? [[:a :b]]))
    (= false (tourable? [[:a :a] [:b :b]]))
    (= false (tourable? [[:a :b] [:a :b] [:a :c] [:c :a]
                         [:a :d] [:b :d] [:c :d]]))
    (= true  (tourable? [[1 2] [2 3] [3 4] [4 1]]))
    (= true  (tourable? [[:a :b] [:a :c] [:c :b] [:a :e]
                         [:b :e] [:a :d] [:b :d] [:c :e]
                         [:d :e] [:c :f] [:d :f]]))
    (= false (tourable? [[1 2] [2 3] [2 4] [2 5]]))))

(defn problem-ninety-one
  "Given a graph, determine whether the graph is connected.
  A connected graph is such that a path exists between any two given nodes.

  -Your function must return true if the graph is connected and false otherwise.

  -You will be given a set of tuples representing the edges of a graph.
  Each member of a tuple being a vertex/node in the graph.

  -Each edge is undirected (can be traversed either direction)."
  []
  (let [connected? (fn [nodes-set]
                     (let [nodes (reduce #(conj (conj %1 (first %2)) (second %2)) #{} nodes-set)
                           nodes-map (into {} (map-indexed #(vector %2 %1) nodes))
                           index-of (fn [node] (get nodes-map node))
                           edges (concat nodes-set (map #(apply vector (reverse %)) nodes-set))
                           grouped-edges (group-by (comp index-of first) edges)
                           destinations (fn [node] (map second (get grouped-edges (index-of node))))
                           walk-graph (fn walk
                                        ([node] (walk #{} (hash-set node)))
                                        ([result walked]
                                         (if (zero? (count walked))
                                           result
                                           (let [current (first walked)
                                                 next-result (conj result current)
                                                 new-nodes (filter #(not (contains? next-result %)) (destinations current))]
                                             (walk next-result (reduce conj (into #{} (rest walked)) new-nodes))))))]
                       (= nodes (walk-graph (ffirst nodes-set)))))]
    (= true  (connected? #{[:a :a]}))
    (= true  (connected? #{[:a :b]}))
    (= false (connected? #{[1 2] [2 3] [3 1]
                           [4 5] [5 6] [6 4]}))
    (= true  (connected? #{[1 2] [2 3] [3 1]
                           [4 5] [5 6] [6 4] [3 4]}))
    (= false (connected? #{[:a :b] [:b :c] [:c :d]
                           [:x :y] [:d :a] [:b :e]}))
    (= true  (connected? #{[:a :b] [:b :c] [:c :d]
                           [:x :y] [:d :a] [:b :e] [:x :a]}))))
