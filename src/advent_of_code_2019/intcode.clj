(ns aoc2019.intcode)


(defn parse-program [input]
  (mapv read-string (clojure.string/split input #",")))


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