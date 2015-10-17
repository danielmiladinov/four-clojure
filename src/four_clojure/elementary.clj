(ns four-clojure.elementary)

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

(defn problem-sixteen
  "Write a function which returns a personalized greeting."
  []
  (= ((fn greetify [name] (str "Hello, " name "!")) "Dave") "Hello, Dave!")
  (= ((fn greetify [name] (str "Hello, " name "!")) "Jenn") "Hello, Jenn!")
  (= ((fn greetify [name] (str "Hello, " name "!")) "Rhea") "Hello, Rhea!"))

(defn problem-seventeen
  "The map function takes two arguments: a function (f) and a sequence (s).
  Map returns a new sequence consisting of the result of applying f to each item of s.
  Do not confuse the map function with the map data structure."
  []
  (= '(6 7 8) (map #(+ % 5) '(1 2 3))))

(defn problem-eighteen
  "The filter function takes two arguments: a predicate function (f) and a sequence (s).
  Filter returns a new sequence consisting of all the items of s for which (f item) returns true."
  []
  (= '(6 7) (filter #(> % 5) '(3 4 5 6 7))))

(defn problem-thirty-five
  "Clojure lets you give local names to values using the special let-form."
  []
  (= 7 (let [x 5] (+ 2 x)))
  (= 7 (let [x 3, y 10] (- y x)))
  (= 7 (let [x 21] (let [y 3] (/ x y)))))

(defn problem-thirty-six
  "Can you bind x, y, and z so that these are all true?"
  []
  (= 10 (let [x 7 y 3 z 1] (+ x y)))
  (= 4  (let [x 7 y 3 z 1] (+ y z)))
  (= 1  (let [x 7 y 3 z 1] z)))

(defn problem-thirty-seven
  "Regex patterns are supported with a special reader macro."
  []
  (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))
