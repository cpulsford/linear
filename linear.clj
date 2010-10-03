;;
;; Cameron Pulsford
;; A simple linear algebra lib
;;

;;
;; helpers
;;

(defn- det-2x2 [[[a b] [c d]]]
  (- (* (int a) (int d))
     (* (int b) (int c))))

(defn drop-at [n m]
  (concat (take (int n) m) (drop (inc (int n)) m)))

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
  (if [(square? m)]
    ((fn self [m] (if (= (dimensions m) [2 2])
                    (det-2x2 m)
                    (reduce + (map * (first m)
                                     (cycle [1 -1])
                                     (map self (minors m)))))) m)
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
    (if (zero? d)
      (throw (Error. (format "Non-invertible matrix.")))
      (for [row adj] (for [col row] (* (/ 1 d) col))))))

;;
;; tests
;;

(defn mat-test [x]
  (mul x (inverse x)))

(mat-test [[1 2 3 4] [4 5 6 1] [7 8 -9 3] [1 1 1 1]])
(mat-test [[1 2 3] [4 5 6] [7 8 -9]])
(mat-test [[1 2 3] [4 5 6] [7 8 9]])

