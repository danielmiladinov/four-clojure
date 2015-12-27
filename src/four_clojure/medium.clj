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

(defn problem-seventy-four
  "Given a string of comma separated integers,
  write a function which returns a new comma separated string that only contains the numbers which are perfect squares."
  []
  (let [filter-perfect-squares (fn [s]
                                 (let [perfect-square? (fn [i]
                                                         (some
                                                           #(= i %)
                                                           (map
                                                             #(* % %)
                                                             (range (dec i) 1 -1))))]
                                   (->> (clojure.string/split s #",")
                                        (map #(Integer/parseInt %))
                                        (filter perfect-square?)
                                        (clojure.string/join ","))))]

    (= (filter-perfect-squares "4,5,6,7,8,9") "4,9")
    (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

(defn problem-seventy-five
  "Two numbers are coprime if their greatest common divisor equals 1.
  Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x.
  The special case f(1) equals 1. Write a function which calculates Euler's totient function."
  []
  (let [euler-totient (fn [x]
                        (let [gcd (fn [a b] (if (= b 0) a (recur b (mod a b))))
                              coprime? (fn [x y] (= 1 (gcd x y)))]
                          (cond
                            (= x 1) 1
                            :else (->> (range 1 x)
                                       (filter (partial coprime? x))
                                       (count)))))]
    (= (euler-totient 1) 1)
    (= (euler-totient 10) (count '(1 3 7 9)) 4)
    (= (euler-totient 40) 16)
    (= (euler-totient 99) 60)))

(defn problem-seventy-six
  "The trampoline function takes a function f and a variable number of parameters.
  Trampoline calls f with any parameters that were supplied. If f returns a function,
  trampoline calls that function with no arguments. This is repeated, until the return value is not a function,
  and then trampoline returns that non-function value. This is useful for implementing mutually recursive algorithms
  in a way that won't consume the stack."
  []
  (= [1 3 5 7 9 11]
     (letfn
       [(foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                     x
                     #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))

(defn problem-seventy-seven
  "Write a function which finds all the anagrams in a vector of words.
  A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y.
  Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other.
  Each sub-set should have at least two words. Words without any anagrams should not be included in the result."
  []
  (let [anagrams (fn [words]
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
                                   arr))))]
                     (let [permutations (map permute words)]
                       (->>
                         (map (fn [permuted]
                                (->>
                                  (map #(clojure.string/join "" %) permuted)
                                  (filter (set words)))) permutations)
                         (filter #(> (count %) 1))
                         (map set)
                         (set)))))]
    (= (anagrams ["meat" "mat" "team" "mate" "eat"])
       #{#{"meat" "team" "mate"}})
    (= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
       #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

(defn problem-seventy-eight
  "Reimplement the function described in 'Intro to Trampoline'."
  []
  (let [my-tramp (fn [f & args]
                   (loop [thunk (apply f args)]
                     (if (fn? thunk)
                       (recur (thunk))
                       thunk)))]
    (= (letfn [(triple [x] #(sub-two (* 3 x)))
               (sub-two [x] #(stop? (- x 2)))
               (stop? [x] (if (> x 50) x #(triple x)))]
         (my-tramp triple 2))
       82)
    (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
               (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
         (map (partial my-tramp my-even?) (range 6)))
       [true false true false true false])))

(defn problem-eighty
  "A number is 'perfect' if the sum of its divisors equal the number itself.
  6 is a perfect number because 1+2+3=6.
  Write a function which returns true for perfect numbers and false otherwise."
  []
  (let [perfect? (fn [num]
                   (->> (range (/ num 2))
                        (filter #(= 0 (mod num %)))
                        (apply +)
                        (= num)))]
    (= (perfect? 6) true)
    (= (perfect? 7) false)
    (= (perfect? 496) true)
    (= (perfect? 500) false)))

(defn problem-eighty-five
  "Write a function which generates the power set of a given set.
  The power set of a set x is the set of all subsets of x, including the empty set and x itself."
  []
  (let [power-set (fn [s]
                    (reduce (fn [ps e]
                              (->>
                                ps
                                (map #(set (concat #{e} %)))
                                (concat ps)
                                set))
                            #{#{}}
                            s))]
    (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
    (= (power-set #{}) #{#{}})
    (= (power-set #{1 2 3})
       #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
    (= (count (power-set (into #{} (range 10)))) 1024)))

(defn problem-eighty-six
  "Happy numbers are positive integers that follow a particular formula: take each individual digit, square it,
  and then sum the squares to get a new number. Repeat with the new number and eventually,
  you might get to a number whose squared sum is 1. This is a happy number.
  An unhappy number (or sad number) is one that loops endlessly.
  Write a function that determines if a number is happy or not."
  []
  (let [happy-number? (fn [number]
                        (let [square (fn [x] (let [y (Integer/parseInt (.toString x))] (* y y)))
                              happy  (fn [x] (->> x (str) (seq) (map square) (apply +)))]
                          (loop [seen #{}, n (happy number)]
                            (if (and (not (seen n)) (> n 1))
                              (recur (conj seen n) (happy n))
                              (= 1 n)))))]
    (= (happy-number? 7) true)
    (= (happy-number? 986543210) true)
    (= (happy-number? 2) false)
    (= (happy-number? 3) false)))

(defn problem-ninety-three
  "Write a function which flattens any nested combination of sequential things (lists, vectors, etc.),
  but maintains the lowest level sequential items.
  The result should be a sequence of sequences with only one level of nesting."
  []
  (let [partial-flatten (fn partial-flatten [coll]
                          (if (every? sequential? coll)
                            (mapcat partial-flatten coll)
                            [coll]))]
    (= (partial-flatten [["Do"] ["Nothing"]])
       [["Do"] ["Nothing"]])
    (= (partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]])
       [[:a :b] [:c :d] [:e :f]])
    (= (partial-flatten '((1 2) ((3 4) ((((5 6)))))))
       '((1 2)(3 4)(5 6)))))

(defn problem-ninety-eight
  "A function f defined on a domain D induces an equivalence relation on D, as follows:
  a is equivalent to b with respect to f if and only if (f a) is equal to (f b).
  Write a function with arguments f and D that computes the equivalence classes of D with respect to f."
  []
  (let [equivalence-classes (fn [f d]
                              (->> (group-by f d)
                                   (vals)
                                   (map set)
                                   (set)))]
    (= (equivalence-classes #(* % %) #{-2 -1 0 1 2})
       #{#{0} #{1 -1} #{2 -2}})
    (= (equivalence-classes #(rem % 3) #{0 1 2 3 4 5})
       #{#{0 3} #{1 4} #{2 5}})
    (= (equivalence-classes identity #{0 1 2 3 4})
       #{#{0} #{1} #{2} #{3} #{4}})
    (= (equivalence-classes (constantly true) #{0 1 2 3 4})
       #{#{0 1 2 3 4}})))

(defn problem-one-hundred-two
  "When working with java, you often need to create an object with fieldsLikeThis,
  but you'd rather work with a hashmap that has :keys-like-this until it's time to convert.
  Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings."
  []
  (let [into-camel-case (fn [s]
                          (let [parts (clojure.string/split s #"-")
                                ucfirst (fn [w]
                                          (str
                                            (.toUpperCase (str (first w)))
                                            (apply str (rest w))))]
                            (str (first parts) (map ucfirst (rest parts)))))]
    (= (into-camel-case "multi-word-key") "multiWordKey")
    (= (into-camel-case "something") "something")
    (= (into-camel-case "leaveMeAlone") "leaveMeAlone")))

(defn problem-one-hundred-three
  "Given a sequence S consisting of n elements generate all k-combinations of S,
  i. e. generate all possible sets consisting of k distinct elements taken from S.
  The number of k-combinations for a sequence is equal to the binomial coefficient."
  []
  (let [k-combinations (fn [k S]
                         (let [power-set (fn [s]
                                           (reduce (fn [ps e]
                                                     (->>
                                                       ps
                                                       (map #(set (concat #{e} %)))
                                                       (concat ps)
                                                       set))
                                                   #{#{}}
                                                   s))]
                           (into #{}
                                 (filter #(= (count %) k) (power-set S)))))]
    (= (k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}})
    (= (k-combinations 10 #{4 5 6}) #{})
    (= (k-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
    (= (k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
    (= (k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
    (= (k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                      #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

(defn problem-one-hundred-four
  "This is the inverse of Problem 92, but much easier.
  Given an integer smaller than 4000, return the corresponding roman numeral in uppercase,
  adhering to the subtractive principle."
  []
  (let [to-roman (fn [arabic]
                   (let [ones           [1    \I \V \X]
                         tens           [10   \X \L \C]
                         hundreds       [100  \C \D \M]
                         thousands      [1000 \M]
                         symbol-pattern (fn ([ones-symbol times]
                                             (apply str (repeat times ones-symbol)))
                                            ([ones-symbol fives-symbol tens-symbol magnitude-value]
                                             (let [pattern {1 [ones-symbol                                      ]
                                                            2 [ones-symbol  ones-symbol                         ]
                                                            3 [ones-symbol  ones-symbol  ones-symbol            ]
                                                            4 [ones-symbol  fives-symbol                        ]
                                                            5 [fives-symbol                                     ]
                                                            6 [fives-symbol ones-symbol                         ]
                                                            7 [fives-symbol ones-symbol  ones-symbol            ]
                                                            8 [fives-symbol ones-symbol  ones-symbol ones-symbol]
                                                            9 [ones-symbol  tens-symbol                         ]}]
                                               (apply str (get pattern magnitude-value)))))]
                     (apply
                       str
                       (map
                         (fn get-symbol [pattern]
                           (let [[magnitude & chars] pattern
                                 value (if (>= arabic magnitude)
                                         (quot (mod arabic (* magnitude 10)) magnitude)
                                         0)
                                 args (conj (vec chars) value)]
                             (apply symbol-pattern args)))
                         [thousands
                          hundreds
                          tens
                          ones]))))]
    (= "I" (to-roman 1))
    (= "XXX" (to-roman 30))
    (= "IV" (to-roman 4))
    (= "CXL" (to-roman 140))
    (= "DCCCXXVII" (to-roman 827))
    (= "MMMCMXCIX" (to-roman 3999))
    (= "XLVIII" (to-roman 48))))

(defn problem-one-hundred-five
  "Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword,
  and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence."
  []
  (let [mapify-keywords (fn [coll]
                          (->> coll
                               (reduce
                                 (fn fold-seq-into-map [[map last-keyword] next-value]
                                   (if (keyword? next-value)
                                     [(assoc map next-value []) next-value]
                                     [(update-in map [last-keyword] #(conj % next-value)) last-keyword]))
                                 [{} nil])
                               (first)))]
    (= {} (mapify-keywords []))
    (= {:a [1]} (mapify-keywords [:a 1]))
    (= {:a [1], :b [2]} (mapify-keywords [:a 1, :b 2]))
    (= {:a [1 2 3], :b [], :c [4]} (mapify-keywords [:a 1 2 3 :b :c 4]))))

(fn [coll]
  (->> coll
       (reduce
         (fn fold-seq-into-map [[map last-keyword] next-value]
           (if (keyword? next-value)
             [(assoc map next-value []) next-value]
             [(update-in map [last-keyword] #(conj % next-value)) last-keyword]))
         [{} nil])
       (first)))

(defn problem-one-hundred-eight
  "Given any number of sequences, each sorted from smallest to largest,
  find the smallest single number which appears in all of the sequences.
  The sequences may be infinite, so be careful to search lazily."
  []
  (let [search-lazily (fn [& seqs]
                        (if (= 1 (count seqs))
                          (ffirst seqs)

                          (loop [lazy-seqs (vec (map #(lazy-seq %) seqs))
                                 lazy-seen (vec (map (comp sorted-set first) lazy-seqs))]

                            (let [intersection (apply clojure.set/intersection lazy-seen)]

                              (if (not (empty? intersection))

                                (first intersection)

                                (let [[min-seq-idx _] (->> (map-indexed #(vector %1 (apply max %2)) lazy-seen)
                                                           (sort-by second)
                                                           (first))
                                      curr-set (lazy-seen min-seq-idx)
                                      curr-seq (lazy-seqs min-seq-idx)
                                      next-seq (lazy-seq (rest curr-seq))
                                      next-val (first next-seq)
                                      next-set (conj curr-set next-val)]

                                  (recur (assoc lazy-seqs min-seq-idx next-seq)
                                         (assoc lazy-seen min-seq-idx next-set))))))))]
    (= 3 (search-lazily [3 4 5]))
    (= 4 (search-lazily [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
    (= 7 (search-lazily (range) (range 0 100 7/6) [2 3 5 7 11 13]))
    (= 64 (search-lazily (map #(* % % %) (range))                       ;; perfect cubes
                         (filter #(zero? (bit-and % (dec %))) (range))  ;; powers of 2
                         (iterate inc 20)))))                           ;; at least as large as 20
