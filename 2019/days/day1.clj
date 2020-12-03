

(defn calculate-fuel-requirement [mass] 
    (- (int (Math/floor (/ mass 3))) 2))

(defn read-in-input [filename]
    (clojure.string/split-lines 
        (slurp filename)))

;; First solution
(reduce +
    (map #(calculate_fuel_requirement %) 
        (map #(Integer/parseInt %) (read-in-input "inputs/day01_01.txt"))))

(defn get-total-fuel [fuel-mass]
    (if (> fuel-mass 0)
        (+ fuel-mass (get-total-fuel (calculate-fuel-requirement fuel-mass)))
        0
    ))


(get-total-fuel 654)
(get-total-fuel 33583)

;; Second solution
(reduce +
    (map #(get-total-fuel %)
        (map #(calculate_fuel_requirement %) 
            (map #(Integer/parseInt %) (read-in-input "inputs/day01_01.txt")))))