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
                       (let [c (seq coll)]
                         (first
                           (reduce
                             (fn s [[acc pred] e]
                               (if (pred e)
                                 [(conj acc e) (fn [x] (not= e x))]
                                 [acc pred]))
                             [[] identity]
                             c))))]
    (= (apply str (filter-dupes "Leeeeeerrroyyy")) "Leroy")
    (= (filter-dupes [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
    (= (filter-dupes [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))
