(ns aoc2019.day4)


(defn parse-input [input]
  (map read-string (clojure.string/split input #"-")))


(defn password-validator [rules]
  (fn [password]
    (every? true? (map #(% password) rules))))


(defn double-digits? 
  "Implementation for part 1. Any digit that occurs more than once is considered valid.

   A set (#{}) automatically de-duplicates anything put into it, so if there are duplicate digits,
   the set will contain less items than the number itself."
  [n]
  (let [s (str n)
        digits (into #{} s)]
    (not= (count s) (count digits))))


(defn double-digits-2? 
  "Implementation for part 2. Only digits that occur exactly twice are considered valid.

   I create a map which counts the occurence of each digit within the number, then look
   for any digit occurring exactly twice."
  [n]
  (let [digit-count (reduce (fn [m c]
                              (update m c #(inc (or % 0)))) 
                            {} 
                            (str n))]
    (some #(= 2 %) (vals digit-count))))


(defn never-decreases? [n]
  (let [digits (into [] (str n))]
    (= (apply list digits) (sort digits))))


(defn generate-valid-passwords [input rules]
  (let [[start end] (parse-input input)]
    (filter (password-validator rules) (range start (inc end)))))


(defn part-1 [input]
  (let [rules [never-decreases? double-digits?]]
    (count (generate-valid-passwords input rules))))


(defn part-2 [input]
  (let [rules [never-decreases? double-digits-2?]]
    (count (generate-valid-passwords input rules))))


(def input "357253-892942")