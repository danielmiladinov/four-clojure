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
