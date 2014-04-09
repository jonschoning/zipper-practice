(ns zipper-practice.core
  (:require
    [clojure.data.xml :as xml]
    [clojure.java.io :refer [resource input-stream]]
    [clojure.pprint :refer [pprint]]
    [clojure.zip :as zip]
    [fast-zip.core :as fast-zip]))

(def parse-tree
  "Representation of an arithmetic expression as a nested list of
  symbols"
  (list (list 'a '* 'b) '+ (list 'c '* 'd)))

(defn parse-xml-resource
  "Parse a XML resource to Clojure data structures."
  [n]
  (-> (resource n) (input-stream) (xml/parse)))

(def nested-maps
  "Representation of a tree in which branches are unordered and can be
  looked up by key"
  {:a {:b {}
       :c 1}
   :d {:e 2
       :f 3
       :g 4}
   :h {:i 5
       :j {:k 6
           :l {:m 7
               :n 8
               :o 9}
           :p 10}}})

(defn- print-location
  [location]
  (println
    "Focused sub-tree is" (first location))
  (println
    "Path is\n" (with-out-str (pprint (second location)))))

(defn divisors
  "Return a lazy seq of integers that exactly divide n, in order from
  largest to smallest.

  This function uses a stupidly naive brute-force approach."
  [n]
  (when-not (and (integer? n) (pos? n))
    (throw
      (IllegalArgumentException. "n must be a positive integer")))
  (filter #(integer? (/ n %)) (range n 0 -1)))

(defn divisors-tree-zipper
  "Return a zipper to traverse an infinite lazy tree of integer divisors
  of n."
  [n]
  (zip/zipper
    (constantly true) ; branch? function
    divisors ; children function
    (fn [_ _] ; make-node function
      (throw (UnsupportedOperationException. "This tree is read-only")))
    n)) ; root is just the integer n

(defn -main
  [& _]
  (println)
  (println
    "Using a zipper on the parse tree for the arithmetic expression"
    parse-tree)
  (println)
  (println "Starting traversal at the top of the tree\n")
  (let [initial-location (zip/seq-zip parse-tree)]
    (print-location initial-location)
    (loop [location initial-location
           steps [{:f #'zip/down}
                  {:f #'zip/rightmost}
                  {:f #'zip/down}
                  {:f #'zip/right}
                  {:f #'zip/insert-left :args ['*]}
                  {:f #'zip/insert-left :args ['q]}]]
      (if (seq steps)
        (let [{:keys [f args] :or {args []}} (first steps)
              new-location (apply @f location args)]
          (Thread/sleep 1000)
          (println
            (format
              "Called %s, which returned a new location\n"
              (apply list (:name (meta f)) 'location args)))
          (print-location new-location)
          (recur new-location (next steps)))
        (println "Final modified result is" (zip/root location))))))
