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

(defn problem-ninety-two
  "Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them.
  Write a function to parse a Roman-numeral string and return the number it represents.

  You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle.
  You don't need to handle any numbers greater than MMMCMXCIX (3999),
  the largest number representable with ordinary letters."
  []
  (let [to-arabic (fn [roman]
                    (let [in-arabic {\I 1,
                                     \V 5,
                                     \X 10,
                                     \L 50,
                                     \C 100,
                                     \D 500,
                                     \M 1000 }
                          chars (seq roman)]
                      (first
                        (reduce
                          (fn [[sum prev-r] curr-r]
                            (let [curr-a (in-arabic curr-r)
                                  prev-a (in-arabic prev-r)]
                              (if (nil? prev-r)
                                [curr-a curr-r]
                                (let [next-a   (if (> curr-a prev-a) (- curr-a (* 2 prev-a)) curr-a)
                                      next-sum (+ sum next-a)]
                                  [next-sum curr-r]))))
                          [0 nil]
                          chars))))]
    (= 14   (to-arabic "XIV"))
    (= 827  (to-arabic "DCCCXXVII"))
    (= 3999 (to-arabic "MMMCMXCIX"))
    (= 48   (to-arabic "XLVIII"))))

(defn problem-ninety-four
  "The game of life is a cellular automaton devised by mathematician John Conway.

  The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with its eight neighbours
  (horizontal, vertical, diagonal), and its next state is dependent on the following rules:

  1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  2) Any live cell with two or three live neighbours lives on to the next generation.
  3) Any live cell with more than three live neighbours dies, as if by overcrowding.
  4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

  Write a function that accepts a board, and returns a board representing the next generation of cells."
  []
  (let [life (fn [board]
               (let [live-cell \#
                     dead-cell \space
                     live? (fn [cell] (= live-cell cell))
                     neighbors (fn [x y]
                                 (let [spread (fn [x] [(dec x) x (inc x)])
                                       coords (filter
                                                #(not= [x y] %)
                                                (for [a (spread x)
                                                      b (spread y)]
                                                  [a b]))]
                                   (filter (complement nil?)
                                           (map
                                             (fn [[a b]]
                                               (when-let [_ (contains? board a)]
                                                 (when-let [_ (contains? (vec (board a)) b)]
                                                   ((vec (board a)) b))))
                                             coords))))]
                 (map-indexed
                   (fn [r row]
                     (apply
                       str
                       (map-indexed
                         (fn [c cell]
                           (let [live-neighbors (count (filter live? (neighbors r c)))]
                             (if (live? cell)
                               (cond
                                 (< live-neighbors 2) dead-cell
                                 (> live-neighbors 3) dead-cell
                                 :else live-cell)
                               (if (= 3 live-neighbors)
                                 live-cell
                                 dead-cell))))
                         row)))
                   board)))]
    (= (life ["      "
              " ##   "
              " ##   "
              "   ## "
              "   ## "
              "      "])
       ["      "
        " ##   "
        " #    "
        "    # "
        "   ## "
        "      "])
    (= (life ["     "
              "     "
              " ### "
              "     "
              "     "])
       ["     "
        "  #  "
        "  #  "
        "  #  "
        "     "])
    (= (life ["      "
              "      "
              "  ### "
              " ###  "
              "      "
              "      "])
       ["      "
        "   #  "
        " #  # "
        " #  # "
        "  #   "
        "      "])))

(defn problem-one-hundred-one
  "Given two sequences x and y, calculate the Levenshtein distance of x and y,
  i. e. the minimum number of edits needed to transform x into y.
  The allowed edits are:

  - insert a single item
  - delete a single item
  - replace a single item with another item

  WARNING: Some of the test cases may timeout if you write an inefficient solution!"
  []
  (let [levenshtein (fn [w1 w2]
                      (letfn [(cell-value [same-char? prev-row cur-row col-idx]
                                (min (inc (nth prev-row col-idx))
                                     (inc (last cur-row))
                                     (+ (nth prev-row (dec col-idx)) (if same-char? 0 1))))]
                        (loop [row-idx 1
                               max-rows (inc (count w2))
                               prev-row (range (inc (count w1)))]
                          (if (= row-idx max-rows)
                            (last prev-row)
                            (let [ch2 (nth w2 (dec row-idx))
                                  next-prev-row (reduce (fn [cur-row i]
                                                          (let [same-char? (= (nth w1 (dec i)) ch2)]
                                                            (conj cur-row (cell-value same-char?
                                                                                      prev-row
                                                                                      cur-row
                                                                                      i))))
                                                        [row-idx] (range 1 (count prev-row)))]
                              (recur (inc row-idx) max-rows next-prev-row))))))]
    (= (levenshtein "kitten" "sitting") 3)
    (= (levenshtein "closure" "clojure")
       (levenshtein "clojure" "closure") 1)
    (= (levenshtein "xyx" "xyyyx") 2)
    (= (levenshtein "" "123456") 6)
    (= (levenshtein "Clojure" "Clojure")
       (levenshtein "" "")
       (levenshtein [] []) 0)
    (= (levenshtein [1 2 3 4] [0 2 3 4 5]) 2)
    (= (levenshtein '(:a :b :c :d) '(:a :d)) 2)
    (= (levenshtein "ttttattttctg" "tcaaccctaccat") 10)
    (= (levenshtein "gaattctaatctc" "caaacaaaaaattt") 9)))

(defn problem-one-hundred-six
  "Given a pair of numbers, the start and end point, find a path between the two using only three possible operations:

  * double
  * halve (odd numbers cannot be halved)
  * add 2

  Find the shortest path through the 'maze'.
  Because there are multiple shortest paths, you must return the length of the shortest path, not the path itself."
  []
  (let [shortest-path-length (fn [a b]
                               (letfn [(double   [x] (* 2 x))
                                       (halve    [x] (if (zero? x) x (when (even? x) (quot x 2))))
                                       (add-two  [x] (+ 2 x))
                                       (next-leg [x]
                                         (map #(% x)
                                              (if (even? x)
                                                [double halve add-two]
                                                [double add-two])))
                                       (search [seen goal length]
                                         (if (seen goal)
                                           length
                                           (recur
                                             (reduce into #{} (map next-leg seen))
                                             goal
                                             (inc length))))]
                                 (search #{a} b 1)))]
    (= 1 (shortest-path-length 1 1))                        ; 1
    (= 3 (shortest-path-length 3 12))                       ; 3 6 12
    (= 3 (shortest-path-length 12 3))                       ; 12 6 3
    (= 3 (shortest-path-length 5 9))                        ; 5 7 9
    (= 9 (shortest-path-length 9 2))                        ; 9 18 20 10 12 6 8 4 2
    (= 5 (shortest-path-length 9 12))))                     ; 9 11 22 24 12)

(defn problem-one-hundred-eleven
  "Write a function that takes a string and a partially-filled crossword puzzle board,
  and determines if the input string can be legally placed onto the board.

  The crossword puzzle board consists of a collection of partially-filled rows.
  Empty spaces are denoted with an underscore (_), unusable spaces are denoted with a hash symbol (#),
  and pre-filled spaces have a character in place; the whitespace characters are for legibility and should be ignored.

  For a word to be legally placed on the board:
  - It may use empty spaces (underscores)
  - It may use but must not conflict with any pre-filled characters.
  - It must not use any unusable spaces (hashes).
  - There must be no empty spaces (underscores) or extra characters before or after the word
    (the word may be bound by unusable spaces though).
  - Characters are not case-sensitive.
  - Words may be placed vertically (proceeding top-down only), or horizontally (proceeding left-right only)."
  []
  (let [word-fits? (fn [word board]
                     (let [vectorize (fn [row-string]
                                       (->> (clojure.string/split row-string #"")
                                            (remove #(= " " %))
                                            (remove empty?)
                                            (vec)))

                           rows (map vectorize board)
                           cols (map (fn [i]
                                       (vec
                                         (map #(nth % i) rows)))
                                     (range (count (first rows))))

                           letters (vectorize word)
                           word-length (count letters)

                           slots (->> (concat rows cols)
                                      (mapcat (partial partition-by (partial = "#")))
                                      (remove (partial = ["#"]))
                                      (remove #(not= word-length (count %)))
                                      (map vec)
                                      (vec))

                           pairs-series (map
                                          (fn zip-word-and-slot [slot]
                                            (map vector letters slot))
                                          slots)

                           solutions (map
                                       (fn pairs-valid? [pairs]
                                         (every?
                                           (fn pair-valid? [[wrd slt]]
                                             (or (= slt "_")
                                                 (= wrd slt))) pairs))
                                       pairs-series)]

                       (boolean (some true? solutions))))]

    (= true (word-fits? "the" ["_ # _ _ e"]))

    (= false (word-fits? "the" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"]))

    (= true (word-fits? "joy" ["c _ _ _"
                               "d _ # e"
                               "r y _ _"]))

    (= false (word-fits? "joy" ["c o n j"
                                "_ _ y _"
                                "r _ _ #"]))

    (= true (word-fits? "clojure" ["_ _ _ # j o y"
                                   "_ _ o _ _ _ _"
                                   "_ _ f _ # _ _"]))))

(defn problem-one-hundred-thirteen
  "Write a function that takes a variable number of integer arguments.
  If the output is coerced into a string, it should return a comma (and space)
  separated list of the inputs sorted smallest to largest.
  If the output is coerced into a sequence, it should return
  a seq of unique input elements in the same order as they were entered.\n"
  []
  (let [dance-data (fn [& nums]
                     (reify
                       clojure.lang.ISeq
                       (seq [_] (seq (distinct nums)))
                       (toString [_] (->> (sort nums)
                                          (clojure.string/join ", ")))))]
    (= "1, 2, 3" (str (dance-data 2 1 3)))
    (= '(2 1 3) (seq (dance-data 2 1 3)))
    (= '(2 1 3) (seq (dance-data 2 1 3 3 1 2)))
    (= '(1) (seq (apply dance-data (repeat 5 1))))
    (= "1, 1, 1, 1, 1" (str (apply dance-data (repeat 5 1))))
    (and (= nil (seq (dance-data)))
         (=  "" (str (dance-data))))))

(defn problem-one-hundred-seventeen
  "A mad scientist with tenure has created an experiment tracking mice in a maze.
  Several mazes have been randomly generated, and you've been tasked with writing a program to determine the mazes
  in which it's possible for the mouse to reach the cheesy endpoint.
  Write a function which accepts a maze in the form of a collection of rows, each row is a string where:

  * Spaces represent areas where the mouse can walk freely
  * Hashes (#) represent walls where the mouse can not walk
  * M represents the mouse's starting point
  * C represents the cheese which the mouse must reach

  The mouse is not allowed to travel diagonally in the maze (only up/down/left/right), nor can he escape the edge
  of the maze. Your function must return true iff the maze is solvable by the mouse."
  []
  (let [maze-solvable? (fn [maze-rows]
                         (let [maze (vec (map (fn vectorize [row-string]
                                                (->> (clojure.string/split row-string #"")
                                                     (remove empty?)
                                                     (vec)))
                                              maze-rows))

                               coords-of (fn [target]
                                           (->> (map-indexed
                                                  (fn [row-index row]
                                                    (map-indexed
                                                      (fn [col-index cell]
                                                        (when (= target cell)
                                                          [row-index col-index]))
                                                      row))
                                                  maze)
                                                (mapcat #(remove nil? %))
                                                (set)))

                               neighbors-of (fn [cell]
                                              (let [[row col] cell]
                                                (set (concat (for [r [(dec row) (inc row)]] [r col])
                                                             (for [c [(dec col) (inc col)]] [row c])))))

                               adjacent? (fn [c1 c2]
                                           (clojure.set/intersection (neighbors-of c1) (hash-set c2)))

                               any-adjacent? (fn [sources]
                                               (fn [dest]
                                                 (not-every?
                                                   empty?
                                                   (map (partial adjacent? dest) sources))))

                               path-exists? (fn [start goal unexplored]
                                              (let [adjacent-to-start (filter (any-adjacent? start) unexplored)
                                                    adjacent-to-goal (filter (any-adjacent? goal) unexplored)]

                                                (cond
                                                  (empty? unexplored) (boolean (seq (filter (any-adjacent? start) goal)))
                                                  (every? empty? [adjacent-to-start adjacent-to-goal]) false
                                                  :else (recur (into start adjacent-to-start)
                                                               (into goal adjacent-to-goal)
                                                               (clojure.set/difference unexplored adjacent-to-start adjacent-to-goal)))))]

                           (path-exists?
                             (coords-of "M")
                             (coords-of "C")
                             (coords-of " "))))]

    (= true (maze-solvable? ["M   C"]))

    (= false (maze-solvable? ["M # C"]))

    (= true (maze-solvable? ["#######"
                             "#     #"
                             "#  #  #"
                             "#M # C#"
                             "#######"]))

    (= false (maze-solvable? ["########"
                              "#M  #  #"
                              "#   #  #"
                              "# # #  #"
                              "#   #  #"
                              "#  #   #"
                              "#  # # #"
                              "#  #   #"
                              "#  #  C#"
                              "########"]))

    (= false (maze-solvable? ["M     "
                              "      "
                              "      "
                              "      "
                              "    ##"
                              "    #C"]))

    (= true (maze-solvable? ["C######"
                             " #     "
                             " #   # "
                             " #   #M"
                             "     # "]))

    (= true (maze-solvable? ["C# # # #"
                             "        "
                             "# # # # "
                             "        "
                             " # # # #"
                             "        "
                             "# # # #M"]))))

(defn problem-one-hundred-nineteen
  "As in Problem 73, a tic-tac-toe board is represented by a two dimensional vector.
  X is represented by :x, O is represented by :o, and empty is represented by :e.
  Create a function that accepts a game piece and board as arguments,
  and returns a set (possibly empty) of all valid board placements of the game piece
  which would result in an immediate win.

  Board coordinates should be as in calls to get-in. For example, [0 1] is the topmost row, center position."
  []
  (let [winning-moves (fn [player board]
                        (let [coords-of (fn [target]
                                          (->> (map-indexed
                                                 (fn [row-index row]
                                                   (map-indexed
                                                     (fn [col-index cell]
                                                       (when (= target cell)
                                                         [row-index col-index]))
                                                     row))
                                                 board)
                                               (mapcat #(remove nil? %))
                                               (set)))

                              wins? (fn [board]
                                      (let [row (fn [n b] (b n))
                                            col (fn [n b] (into [] (map #(% n) b)))
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
                                            xs (fn [s] (map #(if (= :x %) 1 0) s))
                                            os (fn [s] (map #(if (= :o %) 1 0) s))
                                            won (fn [p]
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
                                          (won xs) (= :x player)
                                          (won os) (= :o player)
                                          :else nil)))]

                          (->> (coords-of :e)
                               (filter #(wins? (assoc-in board % player)))
                               (set))))]

    (= (winning-moves :x [[:o :e :e]
                          [:o :x :o]
                          [:x :x :e]])
       #{[2 2] [0 1] [0 2]})

    (= (winning-moves :x [[:x :o :o]
                          [:x :x :e]
                          [:e :o :e]])
       #{[2 2] [1 2] [2 0]})

    (= (winning-moves :x [[:x :e :x]
                          [:o :x :o]
                          [:e :o :e]])
       #{[2 2] [0 1] [2 0]})

    (= (winning-moves :x [[:x :x :o]
                          [:e :e :e]
                          [:e :e :e]])
       #{})

    (= (winning-moves :o [[:x :x :o]
                          [:o :e :o]
                          [:x :e :e]])
       #{[2 2] [1 1]})))
