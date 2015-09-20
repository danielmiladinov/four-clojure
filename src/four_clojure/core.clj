(ns four-clojure.core)

(defn problem-one
  "This is a clojure form. Enter a value which will make the form evaluate to true.
  Don't over think it! If you are confused, see the getting started page. Hint: true is equal to true."
  []
  (= true true))

(defn problem-two
  "If you are not familiar with polish notation, simple arithmetic might seem confusing."
  []
  (= (- 10 (* 2 3)) 4))

(defn problem-three
  "Clojure strings are Java strings. This means that you can use any of the Java string methods on Clojure strings."
  []
  (= "HELLO WORLD" (.toUpperCase "hello world")))

(defn problem-four
  "Lists can be constructed with either a function or a quoted form."
  []
  (= (list :a :b :c) '(:a :b :c)))

(defn problem-five
  "When operating on a list, the conj function will return a new list with one or more items 'added' to the front.
  Note that there are two test cases, but you are expected to supply only one answer,
  which will cause all the tests to pass."
  []
  (= '(1 2 3 4) (conj '(2 3 4) 1))
  (= '(1 2 3 4) (conj '(3 4) 2 1)))

(defn problem-six
  "Vectors can be constructed several ways. You can compare them with lists."
  []
  (= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))

(defn problem-seven
  "When operating on a Vector, the conj function will return a new vector with one or more items 'added' to the end."
  []
  (= [1 2 3 4] (conj [1 2 3] 4))
  (= [1 2 3 4] (conj [1 2] 3 4)))

(defn problem-eight
  "Sets are collections of unique values."
  []
  (= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))
  (= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d})))

(defn problem-nine
  "When operating on a set, the conj function returns a new set with one or more keys 'added'."
  []
  (= #{1 2 3 4} (conj #{1 4 3} 2)))

(defn problem-ten
  "Maps store key-value pairs. Both maps and keywords can be used as lookup functions. Commas can be used to make
  maps more readable, but they are not required."
  []
  (= 20 ((hash-map :a 10, :b 20, :c 30) :b))
  (= 20 (:b {:a 10, :b 20, :c 30})))

(defn problem-eleven
  "When operating on a map, the conj function returns a new map with one or more key-value pairs 'added'."
  []
  (= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3])))

(defn problem-twelve
  "All Clojure collections support sequencing.
  You can operate on sequences with functions like first, second, and last."
  []
  (= 3 (first '(3 2 1)))
  (= 3 (second [2 3 4]))
  (= 3 (last (list 1 2 3))))

(defn problem-thirteen
  "The rest function will return all the items of a sequence except the first."
  []
  (= [20 30 40] (rest [10 20 30 40]))) ; '(20 30 40) also works

(defn problem-fourteen
  "Clojure has many different ways to create functions."
  []
  (= 8 ((fn add-five [x] (+ x 5)) 3))
  (= 8 ((fn [x] (+ x 5)) 3))
  (= 8 (#(+ % 5) 3))
  (= 8 ((partial + 5) 3)))

(defn problem-fifteen
  "Write a function which doubles a number."
  []
  (= ((fn double-it [x] (* 2 x)) 2) 4)
  (= ((fn double-it [x] (* 2 x)) 3) 6)
  (= ((fn double-it [x] (* 2 x)) 11) 22)
  (= ((fn double-it [x] (* 2 x)) 7) 14))
