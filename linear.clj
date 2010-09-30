;;
;; some linear algebra helpers
;;

(defn matrix
  "user=> (matrix [[1 2 3] [4 5 6] [7 8 9]])
   ((1 2 3) (4 5 6) (7 8 9)) 
   
   user=> (matrix [1 2 3] [4 5 6] [7 8 9])
   ((1 2 3) (4 5 6) (7 8 9))
  
  user=> (matrix [[1 2 3] [4] [7 8 9]])
  Error Illegally sized matrix."
  ([m] (if (apply == (map count m))
         (map seq m)
         (throw (Error. "Illegal sized matrix."))))
  ([m1 & more] (matrix (list* m1 more))))

(defn mat-print [m]
  (println (interpose \newline m)))

(defn transpose [m]
  (apply map list m))

(defn add [m1 m2]
  (map (partial map +) m1 m2))

(defn sub [m1 m2]
  (map (partial map -) m1 m2))

(defn mul [m1 m2]
  (for [a m1]
    (for [b (transpose m2)]
      (reduce + (map * a b)))))

(defn minors [m]
  (letfn [(drop-at [n m]
                   (concat (take n m) (drop (inc n) m)))]
    (let [r (range (count m))]
      (for [a r b r]
        (->> m
            (drop-at a)
            (map #(drop-at b %)))))))

(defn- det-2x2 [[[a b] [c d]]]
  (- (* a d) (* b c)))

(defn- dimensions [m]
  [(count m) (count (first m))])

(defn- square? [m]
  (let [[r c] (dimensions m)]
    (== r c)))

(defn- interleave-signs [x]
  (let [l (count (first x))]
    (loop [i 0 x x coll []]
      (if (seq x)
        (if (even? i)
          (recur (inc i) (rest x) (conj coll (first x) (take l (cycle [1 -1]))))
          (recur (inc i) (rest x) (conj coll (first x) (take l (cycle [-1 1])))))
        coll))))

(defn det [m]
  (if (square? m)
    (letfn [(f [r d] (if (= d [2 2])
                       (det-2x2 m)
                       (reduce + (map * (flatten m)
                                      (cycle [1 -1])
                                      (map det (take r (minors m)))))))]
      (let [[r :as d] (dimensions m)]
        (f r d)))
    (throw (Error. "Can not find the determinant of a non-square matrix."))))

(defn cofactors [m]
  (let [[r] (dimensions m)]
    (for [x (->> (minors m) (map det) (partition r) interleave-signs (partition 2))]
      (apply map * x))))

(defn adjoint [m]
  (-> m cofactors transpose))

(defn inverse [m]
  (let [adj (adjoint m)
        d (det m)]
    (if (not= d 0)
      (for [row adj] (for [col row] (* (/ 1 d) col)))
      (throw (Error. (format "Non-invertible matrix"))))))
