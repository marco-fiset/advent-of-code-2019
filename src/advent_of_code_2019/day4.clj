(ns aoc2019.day4)


(defn parse-input [input]
  (map read-string (clojure.string/split input #"-")))


(defn double-digits? [n]
  (let [s (str n)
        digits (into #{} s)]
    (not= (count s) (count digits))))


(defn double-digits-2? [n]
  (let [digit-count (reduce (fn [m c]
                              (update m c #(inc (or % 0)))) 
                            {} 
                            (str n))]
    (some (partial = 2) (vals digit-count))))


(defn never-decreases? [n]
  (let [digits (into [] (str n))]
    (= (apply list digits) (sort digits))))


(defn password-validator [rules]
  (fn [password]
    (reduce #(and %1 %2) (map #(% password) rules))))


(defn generate-valid-passwords [input rules]
  (let [[start end] (parse-input input)]
    (filter (password-validator rules) (range start end))))


(defn part-1 [input]
  (let [rules [never-decreases? double-digits?]]
    (count (generate-valid-passwords input rules))))


(defn part-2 [input]
  (let [rules [never-decreases? double-digits-2?]]
    (count (generate-valid-passwords input rules))))


(def input "357253-892942")