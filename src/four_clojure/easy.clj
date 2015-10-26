(ns four-clojure.easy)

(defn problem-nineteen
  "Write a function which returns the last element in a sequence."
  []
  (let [my-last (comp first reverse)]
    (= (my-last [1 2 3 4 5]) 5)
    (= (my-last '(5 4 3)) 3)
    (= (my-last ["b" "c" "d"]) "d")))

(defn problem-twenty
  "Write a function which returns the second to last element from a sequence."
  []
  (let [penult (comp second reverse)]
    (= (penult (list 1 2 3 4 5)) 4)
    (= (penult ["a" "b" "c"]) "b")
    (= (penult [[1 2] [3 4]]) [1 2])))

(defn problem-twenty-one
  "Write a function which returns the Nth element from a sequence."
  []
  (let [my-nth (fn [coll n] ((apply comp (cons first (repeat n rest))) coll))]
    (= (my-nth '(4 5 6 7) 2) 6)
    (= (my-nth [:a :b :c] 0) :a)
    (= (my-nth [1 2 3 4] 1) 2)
    (= (my-nth '([1 2] [3 4] [5 6]) 2) [5 6])))

(defn problem-twenty-two
  "Write a function which returns the total number of elements in a sequence."
  []
  (let [my-cnt (fn [coll] (loop [c (seq coll) n 0] (if c (recur (next c) (inc n)) n)))]
    (= (my-cnt '(1 2 3 3 1)) 5)
    (= (my-cnt "Hello World") 11)
    (= (my-cnt [[1 2] [3 4] [5 6]]) 3)
    (= (my-cnt '(13)) 1)
    (= (my-cnt '(:a :b :c)) 3)))

(defn problem-twenty-three
  "Write a function which reverses a sequence."
  []
  (let [my-rev (fn [coll] (reduce conj () (seq coll)))]
    (= (my-rev [1 2 3 4 5]) [5 4 3 2 1])
    (= (my-rev (sorted-set 5 7 2 7)) '(7 5 2))
    (= (my-rev [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])))

(defn problem-twenty-four
  "Write a function which returns the sum of a sequence of numbers."
  []
  (let [my-sum (fn [coll] (reduce + coll))]
    (= (my-sum [1 2 3]) 6)
    (= (my-sum (list 0 -2 5 5)) 8)
    (= (my-sum #{4 2 1}) 7)
    (= (my-sum '(0 0 -1)) -1)
    (= (my-sum '(1 10 3)) 14)))

(defn problem-twenty-five
  "Write a function which returns only the odd numbers from a sequence."
  []
  (let [only-odd (partial filter odd?)]
    (= (only-odd #{1 2 3 4 5}) '(1 3 5))
    (= (only-odd [4 2 1 6]) '(1))
    (= (only-odd [2 2 4 6]) '())
    (= (only-odd [1 1 1 3]) '(1 1 1 3))))

(defn problem-twenty-six
  "Write a function which returns the first X fibonacci numbers."
  []
  (let [my-fibs (fn [n] (take n (map second (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))]
    (= (my-fibs 3) '(1 1 2))
    (= (my-fibs 6) '(1 1 2 3 5 8))
    (= (my-fibs 8) '(1 1 2 3 5 8 13 21))))

(defn problem-twenty-seven
  "Write a function which returns true if the given sequence is a palindrome."
  []
  (let [palindrome? (fn [p] (= (into () p) (seq p)))]
    (false? (palindrome? '(1 2 3 4 5)))
    (true?  (palindrome? "racecar"))
    (true?  (palindrome? [:foo :bar :foo]))
    (true?  (palindrome? '(1 1 3 3 1 1)))
    (false? (palindrome? '(:a :b :c)))))

(defn problem-twenty-eight
  "Write a function which flattens a sequence."
  []
  (let [my-flatten (fn [coll]
                     (seq
                       (reduce
                         (fn r [a e]
                           (
                             (if (sequential? e)
                               (partial reduce r)
                               conj) a e))
                         []
                         coll)))]
    (= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
    (= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c"))
    (= (my-flatten '((((:a))))) '(:a))))

(defn problem-twenty-nine
  "Write a function which takes a string and returns a new string containing only the capital letters."
  []
  (let [filter-caps (fn [s]
                      (clojure.string/join
                        (filter
                          (fn [c]
                            (and
                              (Character/isLetter c)
                              (=
                                (str c)
                                (clojure.string/capitalize c))))
                          s)))]
    (= (filter-caps "HeLlO, WoRlD!") "HLOWRD")
    (empty? (filter-caps "nothing"))
    (= (filter-caps "$#A(*&987Zf") "AZ")))

(defn problem-thirty
  "Write a function which removes consecutive duplicates from a sequence."
  []
  (let [filter-dupes (fn [coll]
                       (->>
                         (seq coll)
                         (reduce
                           (fn s [[acc pred] e]
                             (if (pred e)
                               [(conj acc e) #(not= e %)]
                               [acc pred]))
                           [[] identity])
                         first))]
    (= (apply str (filter-dupes "Leeeeeerrroyyy")) "Leroy")
    (= (filter-dupes [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
    (= (filter-dupes [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

(defn problem-thirty-one
  "Write a function which packs consecutive duplicates into sub-lists."
  []
  (let [pack-duplicates (fn [coll] (partition-by identity coll))]
    (= (pack-duplicates [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
    (= (pack-duplicates [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
    (= (pack-duplicates [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))))

(defn problem-thirty-two
  "Write a function which duplicates each element of a sequence."
  []
  (let [duplicate-elems (fn [coll] (mapcat #(repeat 2 %) coll))]
    (= (duplicate-elems [1 2 3]) '(1 1 2 2 3 3))
    (= (duplicate-elems [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
    (= (duplicate-elems [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
    (= (duplicate-elems [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

(defn problem-thirty-three
  "Write a function which replicates each element of a sequence a variable number of times."
  []
  (let [replicate-elems (fn [coll n] (mapcat #(repeat n %) coll))]
    (= (replicate-elems [1 2 3] 2) '(1 1 2 2 3 3))
    (= (replicate-elems [:a :b] 4) '(:a :a :a :a :b :b :b :b))
    (= (replicate-elems [4 5 6] 1) '(4 5 6))
    (= (replicate-elems [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
    (= (replicate-elems [44 33] 2) [44 44 33 33])))

(defn problem-thirty-four
  "Write a function which creates a list of all integers in a given range."
  []
  (let [my-range (fn [s n] (take (- n s) (iterate inc s)))]
    (= (my-range 1) 4 '(1 2 3))
    (= (my-range -2 2) '(-2 -1 0 1))
    (= (my-range 5 8) '(5 6 7))))

(defn problem-thirty-eight
  "Write a function which takes a variable number of parameters and returns the maximum value."
  []
  (let [my-max (fn [& args] (reduce #(if (< %1 %2) %2 %1) args))]
    (= (my-max 1 8 3 4) 8)
    (= (my-max 30 20) 30)
    (= (my-max 45 67 11) 67)))

(defn problem-thirty-nine
  "Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc."
  []
  (let [my-interleave (fn [c1 c2] (mapcat vector c1 c2))]
    (= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
    (= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
    (= (my-interleave [1 2 3 4] [5]) [1 5])
    (= (my-interleave [30 20] [25 15]) [30 25 20 15])))

(defn problem-forty
  "Write a function which separates the items of a sequence by an arbitrary value."
  []
  (let [my-interpose (fn [s c] (conj (mapcat #(vector s %) (rest c)) (first c)))]
    (= (my-interpose 0 [1 2 3]) [1 0 2 0 3])
    (= (apply str (my-interpose ", " ["one" "two" "three"])) "one, two, three")
    (= (my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

(defn problem-forty-one
  "Write a function which drops every Nth item from a sequence."
  []
  (let [my-drop-nth (fn [coll n]
                      (->> (map-indexed vector coll)
                          (remove #(zero? (mod (inc (first %)) n)))
                          (map second)))]
    (= (my-drop-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
    (= (my-drop-nth [:a :b :c :d :e :f] 2) [:a :c :e])
    (= (my-drop-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])))

(defn problem-forty-two
  "Write a function which calculates factorials."
  []
  (let [my-factorial (fn [x] (reduce * (map inc (range x))))]
    (= (my-factorial 1) 1)
    (= (my-factorial 3) 6)
    (= (my-factorial 5) 120)
    (= (my-factorial 8) 40320)))

(defn problem-forty-five
  "The iterate function can be used to produce an infinite lazy sequence."
  []
  (= [1 4 7 10 13] (take 5 (iterate #(+ 3 %) 1))))

(defn problem-forty-seven
  "The contains? function checks if a KEY is present in a given collection.
  This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists."
  []
  (contains? #{4 5 6} 4)
  (contains? [1 1 1 1 1] 4)
  (contains? {4 :a 2 :b} 4)
  (not (contains? [1 2 4] 4)))
