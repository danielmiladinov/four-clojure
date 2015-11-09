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
