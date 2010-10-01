;;
;; Cameron Pulsford
;; A simple linear algebra lib
;;


;;
;; helpers
;;

(defn- det-2x2 [[[a b] [c d]]]
  (- (* a d) (* b c)))

(defn drop-at [n m]
  (concat (take n m) (drop (inc n) m)))

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

(for [x (range 10) y (range 20)] [x y])

(for [x (range 10)]
  (for [y (range 10)]
    [x y]))

;
;

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

(defn print-mat [m]
  (doseq [x m] (println x)))

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
  (let [r (range (count m))]
    (for [a r b r]
      (->> m
          (drop-at a) ; drop the i'th row
          (map #(drop-at b %)))))) ; drop the j'th column

(defn det [m]
  (if (square? m)
    (if (= (dimensions m) [2 2])
      (det-2x2 m)
      (reduce + (map * (first m)
                       (cycle [1 -1])
                       (map det (minors m)))))
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
