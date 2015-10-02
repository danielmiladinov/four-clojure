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
