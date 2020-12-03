(defn read-opcodes [filename]
    (vec 
        (map #(Integer/parseInt %)
            (clojure.string/split 
                (slurp filename)
                #" "))))

(defn add-opcode 
    "Processes an add opcode and returns an updated vector"
    [pos ops]
    (let [ x (get ops (get ops (+ pos 1)))
           y (get ops (get ops (+ pos 2)))
           result-loc (get ops (+ pos 3))
           result (+ x y)
         ] 
        (into 
            (conj (subvec ops 0 result-loc) result) 
            (subvec ops (+ result-loc 1)))))

(defn mult-opcode 
    "Processes an add opcode and returns an updated vector"
    [pos ops]
    (let [ x (get ops (get ops (+ pos 1)))
            y (get ops (get ops (+ pos 2)))
            result-loc (get ops (+ pos 3))
            result (* x y)
            ] 
        (into 
            (conj (subvec ops 0 result-loc) result) 
            (subvec ops (+ result-loc 1)))))

(defn process-ops [pos ops]
    (case (get ops pos)
       1 (process-ops (+ pos 4) (add-opcode pos ops))
       2 (process-ops (+ pos 4) (mult-opcode pos ops))
       99 ops))

(defn substitute-ops 
    "Replaces the first and second opcodes with the specified noun and verb."
    [ops noun verb]
    (into 
        (vector (get ops 0) noun verb)
        (subvec ops 3)))

(defn get-solution-one []
    (let [ ops (read-opcodes "inputs/day02_01.txt") ]
    (process-ops 0 (substitute-ops ops 12 2))))

(get-solution-one)

(defn get-solution-two []
    (let [ops (read-opcodes "inputs/day02_01.txt")]
        (loop [noun 1
               verb 1]
            (let [processed-ops (process-ops 0 (substitute-ops ops noun verb))]
                (if (or 
                        (= (first processed-ops) 19690720)
                        (= verb 100))
                    (subvec processed-ops 0 3)
                    (recur 
                        (if (= noun 99) 1 (+ noun 1))
                        (if (= noun 99) (+ verb 1) verb)))))))

(get-solution-two)
