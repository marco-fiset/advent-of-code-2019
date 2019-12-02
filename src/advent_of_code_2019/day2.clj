(ns aoc2019.day2)

(declare input)

(defn parse-input [input]
  (mapv read-string (clojure.string/split input #",")))

(defn preprocess-input 
  "before running the program, replace position 1 with the value 12 and replace position 2 with the value 2"  
  [input]
  (-> input
      (assoc 1 12)
      (assoc 2 2)))

(defn step [f instructions pos1 pos2 pos3]
  (let [arg1 (instructions pos1)
        arg2 (instructions pos2)]
    (assoc instructions pos3 (f arg1 arg2))))

(defn run [instructions index]
  (let [[op pos1 pos2 pos3] (drop index instructions)]
    (case op
      99 (first instructions)
      1 (recur (step + instructions pos1 pos2 pos3) (+ 4 index))
      2 (recur (step * instructions pos1 pos2 pos3) (+ 4 index)))))

(defn part-1 [input]
  (let [instructions (-> input
                         parse-input
                         preprocess-input)]
    (run instructions 0)))

(def input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,6,23,27,1,27,5,31,2,9,31,35,1,5,35,39,2,6,39,43,2,6,43,47,1,5,47,51,2,9,51,55,1,5,55,59,1,10,59,63,1,63,6,67,1,9,67,71,1,71,6,75,1,75,13,79,2,79,13,83,2,9,83,87,1,87,5,91,1,9,91,95,2,10,95,99,1,5,99,103,1,103,9,107,1,13,107,111,2,111,10,115,1,115,5,119,2,13,119,123,1,9,123,127,1,5,127,131,2,131,6,135,1,135,5,139,1,139,6,143,1,143,6,147,1,2,147,151,1,151,5,0,99,2,14,0,0")