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
  (let [split-by-type (fn [coll]
                        (map second
                          (reduce
                            (fn [m v]
                              (update-in
                                m
                                [(type v)]
                                #(conj (or %1 []) v))) {} coll)))]
    (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
    (= (set (split-by-type [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]})
    (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))
