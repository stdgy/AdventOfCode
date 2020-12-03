(defn count-orbits [orbits object]
    (loop [obj object 
           acc 0]
        (if (= (get orbits obj) nil)
            acc 
            (recur (get orbits obj) (inc acc)))))

(defn get-solution-one [orbits]
    (reduce + 
        (map #(count-orbits orbits %) (keys orbits))))

(defn read-orbits []
    (reduce 
        merge 
        (map 
            #(assoc {} (second %) (first %))
            (map 
                #(clojure.string/split % (re-pattern "\\)")) 
                (-> "inputs/day06.txt"
                    slurp
                    clojure.string/split-lines)))))

(get-solution-one (read-orbits))

(defn orbit-paths-with-costs [orbits object]
    (loop [obj object 
           cost -1
           paths {}]
        (if (= (get orbits obj) nil)
            paths 
            (recur (get orbits obj) (inc cost) (assoc paths obj cost)))))

(defn get-solution-two [orbits]
    (let [your-paths (orbit-paths-with-costs orbits "YOU")
          santa-paths (orbit-paths-with-costs orbits "SAN")]
        (let [min-intersection-point
                (apply min-key #(+ (get your-paths %) (get santa-paths %))
                        (clojure.set/intersection 
                            (set (keys your-paths))
                            (set (keys santa-paths))))]
            (+ 
                (get your-paths min-intersection-point)
                (get santa-paths min-intersection-point)))))

(get-solution-two (read-orbits))

; Get orbit paths with costs [:A 1 :D 2 :C 3 ...]
; Get orbit paths for you and santa
; Get shared nodes for paths -- Get intersection of keys
; Add costs at shared nodes -- Add costs from intersected node set
; Retrieve minimum cost node -- Return minimum