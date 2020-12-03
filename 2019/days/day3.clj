
(defn parse-path 
    "Takes a list of paths and returns a list of parsed paths of the form {:direction \D :steps 5}"
    [paths]
    (map #(let [path %] {:direction (first path) :steps (Integer/parseInt (subs path 1))}) paths))

(defn get-paths 
    "Returns both paths fully parsed."
    []
    (let [paths (clojure.string/split-lines (slurp "inputs/day03.txt"))]
        (list 
            (parse-path (clojure.string/split (first paths) #","))
            (parse-path (clojure.string/split (second paths) #",")))))

(defn get-next-coord [curr-coord curr-path]
    (case (get curr-path :direction)
        \U (list (first curr-coord) (+ (second curr-coord) 1))
        \D (list (first curr-coord) (- (second curr-coord) 1))
        \R (list (+ (first curr-coord) 1) (second curr-coord))
        \L (list (- (first curr-coord) 1) (second curr-coord))))

(defn collect-coords 
    "Returns a set of coordinates given a sequence of parsed paths."
    [parsed-paths]
    (loop [ curr-path (first parsed-paths)
            rest-paths (rest parsed-paths)
            coords '((0 0))
          ]
        (if (nil? curr-path)
            (set coords) 
            (recur 
                (if (= (get curr-path :steps) 1) 
                    (first rest-paths) 
                    {:direction (get curr-path :direction) :steps (dec (get curr-path :steps))})
                (if (= (get curr-path :steps) 1)
                    (rest rest-paths)
                    rest-paths)
                (let [curr-coord (first coords)]
                    (conj coords (get-next-coord curr-coord curr-path)))))))

(defn collect-coords-w-steps
    "Returns a set of coordinates given a sequence of parsed paths."
    [parsed-paths]
    (loop [ curr-path (first parsed-paths)
            rest-paths (rest parsed-paths)
            coords-map {'(0 0) 0}
            coords '((0 0))
            steps 1
            ]
        (if (nil? curr-path)
            coords-map
            (let [curr-coord (first coords)
                  new-coord (get-next-coord curr-coord curr-path)]
                (recur 
                    (if (= (get curr-path :steps) 1) 
                        (first rest-paths) 
                        {:direction (get curr-path :direction) :steps (dec (get curr-path :steps))})
                    (if (= (get curr-path :steps) 1)
                        (rest rest-paths)
                        rest-paths)
                    (if (contains? coords-map new-coord)
                        coords-map
                        (conj coords-map { new-coord steps }))
                    (conj coords new-coord)
                    (+ steps 1))))))

(defn get-solution-one []
    "Retrieve the first solution."
    (let [parsed-paths (get-paths)]
        (second (sort 
            (map #(+ (Math/abs (first %)) (Math/abs (second %)))
                (clojure.set/intersection 
                    (collect-coords (first parsed-paths))
                    (collect-coords (second parsed-paths))))))))

(defn get-solution-two []
    "Retrieve the second solution."
    (let [parsed-paths (get-paths)
          first-coords (collect-coords-w-steps (first parsed-paths))
          second-coords (collect-coords-w-steps (second parsed-paths))]
        (sort 
            (map #(+ (get first-coords %) (get second-coords %))
                (clojure.set/intersection 
                    (set (keys first-coords))
                    (set (keys second-coords)))))))

(get-solution-two)
(get-solution-one)
