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

(defn problem-forty-eight
  "The some function takes a predicate function and a collection.
  It returns the first logical true value of (predicate x) where x is an item in the collection."
  []
  (= 6 (some #{2 7 6} [5 6 7 8]))
  (= 6 (some #(when (even? %) %) [5 6 7 8])))

(defn problem-forty-nine
  "Write a function which will split a sequence into two parts."
  []
  (let [my-split-at (fn [n coll] [(take n coll) (drop n coll)])]
    (= (my-split-at 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
    (= (my-split-at 1 [:a :b :c :d]) [[:a] [:b :c :d]])
    (= (my-split-at 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(defn problem-fifty-one
  "Here is an example of some more sophisticated destructuring."
  []
  (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d])))

(defn problem-sixty-one
  "Write a function which takes a vector of keys and a vector of values and constructs a map from them."
  []
  (let [build-map (fn [ks vs] (into {} (map vector ks vs)))]
    (= (build-map [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
    (= (build-map [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
    (= (build-map [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(defn problem-sixty-two
  "Given a side-effect free function f and an initial value x
  write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc."
  []
  (let [my-iterate (fn my-iterate [f i] (cons i (lazy-seq (my-iterate f (f i)))))]
    (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16])
    (= (take 100 (my-iterate inc 0)) (take 100 (range)))
    (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(defn problem-sixty-three
  "Given a function f and a sequence s, write a function which returns a map.
  The keys should be the values of f applied to each item in s.
  The value at each key should be a vector of corresponding items in the order they appear in s."
  []
  (let [my-group-by (fn my-group-by [f coll]
                      (reduce
                        (fn [m v]
                          (let [k (f v)]
                            (assoc m k (conj (get m k []) v)))) {} coll))]
    (= (my-group-by #(> % 5) [1 3 6 8])
       {false [1 3], true [6 8]})
    (= (my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
       {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
    (= (my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
       {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(defn problem-sixty-four
  "Reduce takes a 2 argument function and an optional starting value.
  It then applies the function to the first 2 items in the sequence (or the starting value
  and the first element of the sequence). In the next iteration the function will be called
  on the previous return value and the next item from the sequence, thus reducing the entire collection to one value.
  Don't worry, it's not as complicated as it sounds."
  []
  (= 15 (reduce + [1 2 3 4 5]))
  (=  0 (reduce + []))
  (=  6 (reduce + 1 [2 3])))

(defn problem-sixty-six
  "Given two integers, write a function which returns the greatest common divisor."
  []
  (let [my-gcd (fn [a b] (if (= b 0) a (recur b (mod a b))))]
    (= (my-gcd 2 4) 2)
    (= (my-gcd 10 5) 5)
    (= (my-gcd 5 7) 1)
    (= (my-gcd 1023 858) 33)))

(defn problem-eighty-one
  "Write a function which returns the intersection of two sets.
  The intersection is the sub-set of items that each set has in common."
  []
  (let [intersekshun (fn [a b]
                       (reduce #(if (a %2) (conj %1 %2) %1) #{} b))]

    (= (intersekshun #{0 1 2 3} #{2 3 4 5}) #{2 3})
    (= (intersekshun #{0 1 2} #{3 4 5}) #{})
    (= (intersekshun #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

(defn problem-eighty-three
  "Write a function which takes a variable number of booleans.
  Your function should return true if some of the parameters are true, but not all of the parameters are true.
  Otherwise your function should return false."
  []
  (let [half-true? (fn [& args] (boolean (and (some true? args) (some false? args))))]
    (= false (half-true? false false))
    (= true (half-true? true false))
    (= false (half-true? true))
    (= true (half-true? false true false))
    (= false (half-true? true true true))
    (= true (half-true? true true true false))))

(defn problem-eighty-eight
  "Write a function which returns the symmetric difference of two sets.
  The symmetric difference is the set of items belonging to one but not both of the two sets."
  []
  (let [symmetric-difference (fn [x y]
                               (clojure.set/union
                                 (clojure.set/difference x y)
                                 (clojure.set/difference y x)))]
    (= (symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
    (= (symmetric-difference #{:a :b :c} #{}) #{:a :b :c})
    (= (symmetric-difference #{} #{4 5 6}) #{4 5 6})
    (= (symmetric-difference #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(defn problem-ninety
  "Write a function which calculates the Cartesian product of two sets."
  []
  (let [cartesian-product (fn cartesian-product [set-a set-b] (set (for [a set-a b set-b] [a b])))]

    (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
       #{["ace" "♠"] ["ace" "♥"] ["ace" "♦"] ["ace" "♣"]
         ["king" "♠"] ["king" "♥"] ["king" "♦"] ["king" "♣"]
         ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
    (= (cartesian-product #{1 2 3} #{4 5})
       #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
    (= 300 (count (cartesian-product (into #{} (range 10)) (into #{} (range 30)))))))

(defn problem-ninety-five
  "Write a predicate which checks whether or not a given sequence represents a binary tree.
  Each node in the tree must have a value, a left child, and a right child."
  []
  (let [tree? (fn tree? [coll]
                (or (nil? coll)
                    (and (sequential? coll)
                         (= 3 (count coll))
                         (tree? (nth coll 1))
                         (tree? (nth coll 2)))))]
    (= (tree? '(:a (:b nil nil) nil))
       true)
    (= (tree? '(:a (:b nil nil)))
       false)
    (= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
       true)
    (= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
       false)
    (= (tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
       true)
    (= (tree? [1 [2 [3 [4 false nil] nil] nil] nil])
       false)
    (= (tree? '(:a nil ()))
       false)))

(defn problem-ninety-six
  "Let us define a binary tree as 'symmetric' if the left half of the tree is the mirror image
  of the right half of the tree.
  Write a predicate to determine whether or not a given binary tree is symmetric.
  (see To Tree, or not to Tree for a reminder on the tree representation we're using)."
  []
  (let [symmetric? (fn symmetric? [tree]
                     (let [value (fn [t] (nth t 0))
                           left  (fn [t] (nth t 1))
                           right (fn [t] (nth t 2))
                           flip  (fn flip [t]
                                   (if (nil? t)
                                     t
                                     [(value t) (flip (right t)) (flip (left t))]))]
                       (= tree (flip tree))))]
    (= (symmetric? '(:a (:b nil nil) (:b nil nil))) true)
    (= (symmetric? '(:a (:b nil nil) nil)) false)
    (= (symmetric? '(:a (:b nil nil) (:c nil nil))) false)
    (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
            [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
       true)
    (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
            [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
       false)
    (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
            [2 [3 nil [4 [6 nil nil] nil]] nil]])
       false)))

(defn problem-ninety-seven
  "Pascal's triangle is a triangle of numbers computed using the following rules:

  - The first row is 1.
  - Each successive row is computed by adding together adjacent numbers in the row above,
    and adding a 1 to the beginning and end of the row.

  Write a function which returns the nth row of Pascal's Triangle."
  []
  (let [pascal-row (fn pascal-row [n]
                     (cond
                       (= n 1) [1]
                       :else (let [end [1]
                                   middle (map
                                            (partial apply +)
                                            (partition 2 1 (pascal-row (dec n))))]
                               (concat end middle end))))]
    (= (pascal-row 1) [1])
    (= (map pascal-row (range 1 6))
       [    [1]
           [1 1]
          [1 2 1]
         [1 3 3 1]
        [1 4 6 4 1]])
    (= (pascal-row 11)
       [1 10 45 120 210 252 210 120 45 10 1])))

(defn problem-ninety-nine
  "Write a function which multiplies two numbers and returns the result as a sequence of its digits."
  []
  (let [product-digits (fn [a b]
                         (->> (* a b)
                              (str)
                              (seq)
                              (map str)
                              (map #(Integer/parseInt %))))]
    (= (product-digits 1 1) [1])
    (= (product-digits 99 9) [8 9 1])
    (= (product-digits 999 99) [9 8 9 0 1])))

(defn problem-one-hundred
  "Write a function which calculates the least common multiple.
  Your function should accept a variable number of positive integers or ratios."
  []
  (let [least-common-multiple (fn [& args]
                                (let [gcd (fn [a b]
                                            (if (= b 0)
                                              a
                                              (recur b (mod a b))))
                                      lcm (fn [a b]
                                            (/ (* a b) (gcd a b)))]
                                  (reduce lcm args)))]
    (== (least-common-multiple 2 3) 6)
    (== (least-common-multiple 5 3 7) 105)
    (== (least-common-multiple 1/3 2/5) 2)
    (== (least-common-multiple 3/4 1/6) 3/2)
    (== (least-common-multiple 7 5/7 2 3/5) 210)))

(defn problem-one-hundred-seven
  "Lexical scope and first-class functions are two of the most basic building blocks
  of a functional language like Clojure.

  When you combine the two together, you get something very powerful called lexical closures.
  With these, you can exercise a great deal of control over the lifetime of your local bindings,
  saving their values for use later, long after the code you're running now has finished.

  It can be hard to follow in the abstract, so let's build a simple closure.
  Given a positive integer n, return a function (f x) which computes x^n.
  Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined."
  []
  (let [f (fn [n]
            (fn [x]
              (int
                (Math/pow x n))))]
    (= 256 ((f 2) 16),
       ((f 8) 2))
    (= [1 8 27 64] (map (f 3) [1 2 3 4]))
    (= [1 2 4 8 16] (map #((f %) 2) [0 1 2 3 4]))))

(defn problem-one-hundred-eighteen
  "Map is one of the core elements of a functional programming language.
  Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s."
  []
  (let [my-map (fn my-map [f coll]
                 (when-let [s (seq coll)]
                   (lazy-seq (cons (f (first s)) (my-map f (rest s))))))]
    (= [3 4 5 6 7]
       (my-map inc [2 3 4 5 6]))
    (= (repeat 10 nil)
       (my-map (fn [_] nil) (range 10)))
    (= [1000000 1000001]
       (->> (my-map inc (range))
            (drop (dec 1000000))
            (take 2)))))

(defn problem-one-hundred-twenty
  "Write a function which takes a collection of integers as an argument.
  Return the count of how many elements are smaller than the sum of their squared component digits.
  For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared."
  []
  (let [num-less-than-sum-of-squares-of-digits (fn [nums]
                                                    (let [digits-of (fn [num]
                                                                      (->> (clojure.string/split (str num) #"")
                                                                           (remove empty?)
                                                                           (map #(Integer/parseInt %))))
                                                          squares-sum (fn [num]
                                                                        (->> (digits-of num)
                                                                             (map #(* % %))
                                                                             (apply +)))]

                                                      (->> (map #(if (< % (squares-sum %)) 1 0) nums)
                                                           (apply +))))]

    (= 8 (num-less-than-sum-of-squares-of-digits (range 10)))
    (= 19 (num-less-than-sum-of-squares-of-digits (range 30)))
    (= 50 (num-less-than-sum-of-squares-of-digits (range 100)))
    (= 50 (num-less-than-sum-of-squares-of-digits (range 1000)))))
