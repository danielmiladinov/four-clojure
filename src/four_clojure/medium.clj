(ns four-clojure.medium)

(defn problem-forty-three
  "Write a function which reverses the interleave process into x number of subsequences."
  []
  (let [my-rev-interleave (fn [coll n] (apply map list (partition n coll)))]
    (= (my-rev-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
    (= (my-rev-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
    (= (my-rev-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(defn problem-forty-four
  "Write a function which can rotate a sequence in either direction."
  []
  (let [my-rotate (fn [n coll]
                    (->>
                      (split-at (mod n (count coll)) coll)
                      (apply #(concat %2 %1))))]
    (= (my-rotate 2 [1 2 3 4 5]) '(3 4 5 1 2))
    (= (my-rotate -2 [1 2 3 4 5]) '(4 5 1 2 3))
    (= (my-rotate 6 [1 2 3 4 5]) '(2 3 4 5 1))
    (= (my-rotate 1 '(:a :b :c)) '(:b :c :a))
    (= (my-rotate -4 '(:a :b :c)) '(:c :a :b))))

(defn problem-forty-six
  "Write a higher-order function which flips the order of the arguments of an input function."
  []
  (let [flip-args (fn [f]
                    (fn flipped [& args]
                      (apply f (reverse args))))]
    (= 3 ((flip-args nth) 2 [1 2 3 4 5]))
    (= true ((flip-args >) 7 8))
    (= 4 ((flip-args quot) 2 8))
    (= [1 2 3] ((flip-args take) [1 2 3 4 5] 3))))

(defn problem-fifty
  "Write a function which takes a sequence consisting of items with different types and splits them up into a set of
   homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences
   themselves can be returned in any order (this is why 'set' is used in the test cases)."
  []
  (let [split-by-type (fn [coll] (vals (group-by type coll)))]
    (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
    (= (set (split-by-type [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]})
    (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

(defn problem-fifty-four
  "Write a function which returns a sequence of lists of x items each.
  Lists of less than x items should not be returned."
  []
  (let [chunks (fn [x coll]
                 (let [go (fn [all-parts part inputs]
                            (if-let [n (first inputs)]
                              (let [next-part (conj part n)]
                                (if (= x (count next-part))
                                  (recur (conj all-parts next-part) [] (rest inputs))
                                  (recur all-parts, (conj part n) (rest inputs))))
                              all-parts))]
                   (go [] [] coll)))]
    (= (chunks 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
    (= (chunks 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
    (= (chunks 3 (range 8)) '((0 1 2) (3 4 5)))))

(defn problem-fifty-five
  "Write a function which returns a map containing the number of occurences of each distinct item in a sequence."
  []
  (let [occurences (fn [coll] (->> (group-by identity coll)
                                   (map (fn [[x y]] [x (count y)]))
                                   (into {})))]
    (= (occurences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
    (= (occurences [:b :a :b :a :b]) {:a 2, :b 3})
    (= (occurences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

(defn problem-fifty-six
  "Write a function which removes the duplicates from a sequence. Order of the items must be maintained."
  []
  (let [remove-dupes (fn [coll]
                       (->> (reduce
                              (fn [[result uniques] element]
                                (if (not (contains? uniques element))
                                  [(conj result element) (conj uniques element)]
                                  [result uniques]))
                              [[] #{}]
                              coll)
                            (first)))]
    (= (remove-dupes [1 2 1 3 1 2 4]) [1 2 3 4])
    (= (remove-dupes [:a :a :b :b :c :c]) [:a :b :c])
    (= (remove-dupes '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
    (= (remove-dupes (range 50)) (range 50))))

(defn problem-fifty-eight
  "Write a function which allows you to create function compositions.
  The parameter list should take a variable number of functions,
  and create a function that applies them from right-to-left."
  []
  (let [my-compose (fn [& fns]
                     (fn [& args]
                       (first (reduce (fn [acc f] [(apply f acc)]) args (reverse fns)))))]
    (= [3 2 1] ((my-compose rest reverse) [1 2 3 4]))
    (= 5 ((my-compose (partial + 3) second) [1 2 3 4]))
    (= true ((my-compose zero? #(mod % 8) +) 3 5 7 9))
    (= "HELLO" ((my-compose #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

(defn problem-fifty-nine
  "Take a set of functions and return a new function that takes a variable number of arguments
  and returns a sequence containing the result of applying each function left-to-right to the argument list."
  []
  (let [my-juxt (fn [& fs] (fn [& args] (map (fn [f] (apply f args)) fs)))]
    (= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))
    (= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
    (= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

(defn problem-sixty
  "Write a function which behaves like reduce, but returns each intermediate value of the reduction.
  Your function must accept either two or three arguments, and the return sequence must be lazy."
  []
  (let [scan (fn scan
               ([f coll]
                (when-let [s (seq coll)]
                  (scan f (first s) (rest s))))
               ([f init coll]
                (cons init (lazy-seq
                             (when-let [s (seq coll)]
                               (scan f (f init (first s)) (rest s)))))))]
    (= (take 5 (scan + (range))) [0 1 3 6 10])
    (= (scan conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
    (= (last (scan * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

(defn problem-sixty-five
  "Clojure has many sequence types, which act in subtly different ways.
  The core functions typically convert them into a uniform \"sequence\" type and work with them that way,
  but it can be important to understand the behavioral and performance differences so that you know which kind
  is appropriate for your application.
  Write a function which takes a collection and returns one of
  :map, :set, :list, or :vector - describing the type of collection it was given.
  You won't be allowed to inspect their class or use the built-in predicates like list? -
  the point is to poke at them and understand their behavior."
  []
  (let [coll-type (fn [coll]
                    (let [e (empty coll)]
                      (cond
                        (= {} e) :map
                        (= #{} e) :set
                        (reversible? e) :vector
                        :else :list)))]
    (= :map (coll-type {:a 1, :b 2}))
    (= :list (coll-type (range (rand-int 20))))
    (= :vector (coll-type [1 2 3 4 5 6]))
    (= :set (coll-type #{10 (rand-int 5)}))
    (= [:map :set :vector :list] (map coll-type [{} #{} [] ()]))))

(defn problem-sixty-seven
  "Write a function which returns the first x number of prime numbers."
  []

  (let [primes (fn [x] (let [prime? (fn [n]
                                      (cond
                                        (= 1 n) false
                                        (even? n) (= 2 n)
                                        :else
                                        (not
                                          (reduce #(or %1 %2) false
                                                  (map #(= (mod n %) 0)
                                                       (range 3 (+ 1 (Math/sqrt n)) 2))))))
                             next-prime (fn [n]
                                          (cond
                                            (= 1 n) 2
                                            :else
                                            (first
                                              (filter prime?
                                                      (range (+ n 1) (* n 2))))))]
                         (take x (iterate next-prime 2))))]

    (= (primes 2) [2 3])
    (= (primes 5) [2 3 5 7 11])
    (= (last (primes 100)) 541)))

(defn problem-sixty-nine
  "Write a function which takes a function f and a variable number of maps.
  Your function should return a map that consists of the rest of the maps conj-ed onto the first.
  If a key occurs in more than one map, the mapping(s) from the latter (left-to-right)
  should be combined with the mapping in the result by calling (f val-in-result val-in-latter)"
  []
  (let [my-merge-with (fn [f m & ms]
                        (reduce (fn [acc n]
                                  (reduce (fn [r [k v]]
                                            (assoc r k
                                                     (if (contains? r k)
                                                       (f (get r k) v)
                                                       v)))
                                          acc
                                          n))
                                m
                                ms))]
    (= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
       {:a 4, :b 6, :c 20})
    (= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
       {1 7, 2 10, 3 15})
    (= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
       {:a [3 4 5], :b [6 7], :c [8 9]})))

(defn problem-seventy
  "Write a function that splits a sentence up into a sorted list of words.
  Capitalization should not affect sort order and punctuation should be ignored."
  []
  (let [sort-words-ci (fn [words] (->> (clojure.string/split words #"\W+")
                                       (map #(vector %1 (clojure.string/lower-case %1)))
                                       (sort-by second)
                                       (map first)))]
    (= (sort-words-ci "Have a nice day.")
       ["a" "day" "Have" "nice"])
    (= (sort-words-ci "Clojure is a fun language!")
       ["a" "Clojure" "fun" "is" "language"])
    (= (sort-words-ci "Fools fall for foolish follies.")
       ["fall" "follies" "foolish" "Fools" "for"])))
