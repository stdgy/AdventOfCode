(defn num-to-seq [number]
    (vec (map #(Character/digit % 10) (str number)))))

(defn has-same-adjacent-digits
    "Returns true if the number has the same adjacent digits. IE: 0113, 4566, 0098, etc."
    [number]
    (loop [ i 0]
        (let [ 
            x (get number i)
            y (get number (inc i))]
            (if (nil? y)
                false 
                (if (= x y)
                    true 
                    (recur
                        (inc i)))))))

(defn test-has-same-adjacent-digits 
    "Simple set of tests for has-same-adjacent-digits"
    [] 
    (let [test-cases ["12234" "1234" "11234" "12344"]]
        (map #(list % (has-same-adjacent-digits %)) test-cases)))

(test-has-same-adjacent-digits)

(defn does-not-decrease
    "Returns true if a number's consecutive digits never decrease. False otherwise."
    [number]
    (loop [i 0]
        (let [x (get number i) y (get number (inc i))]
        (if (nil? y)
            true 
            (if (< y x)
                false 
                (recur (inc i)))))))

(defn test-does-not-decrease
    "Simple set of tests for does-not-decrease"
    [] 
    (let [test-cases (map num-to-seq '(12345 123445 54321 1233465 555555))]
        (map #(list % (has-same-adjacent-digits %)) test-cases)))

(test-does-not-decrease)


(defn get-solution-one 
    "Get solution one."
    [start end]
    (count (filter #(and 
                        (has-same-adjacent-digits (str %)) 
                        (does-not-decrease (num-to-seq %)))
                (range start (inc end)))))

(defn get-solution-one 
    "Get solution one."
    [start end]
    (count (filter #(does-not-decrease (num-to-seq %))
                (filter #(has-same-adjacent-digits (str %))
                    (range start (inc end))))))

(get-solution-one 171309 643603)


(defn has-matching-digits-modified 
    "Takes a number and returns true if the number has repeated matching digits."
    [number]
    (let [n (str number)]
        (loop [i 0
               last-matched nil
               has-matching-digits false]
            (let [x (get n i)
                  y (get n (inc i))]
            (if (and (nil? y) (nil? x))
                has-matching-digits 
                (if (= x y)
                    (recur (+ i 2) x true)
                    (if (and (not (nil? last-matched)) (= x last-matched))
                        false
                        (recur (inc i) nil has-matching-digits))))))))


(defn has-matching-digits-modified-test 
    "Tests for second round of has-matching-digits."
    []
    (let [nums '(112233 123444 111122)]
        (map #(list % (has-matching-digits-modified %)) nums)))

(has-matching-digits-modified-test)

(defn get-solution-two
    "Get solution two."
    [start end]
    (count (filter #(does-not-decrease (num-to-seq %))
                (filter #(has-matching-digits-modified %)
                    (range start (inc end))))))

(get-solution-two 171309 643603)


