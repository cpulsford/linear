;;
;; Cameron Pulsford
;; A simple linear algebra lib
;;

(ns linear
  (use utils))

(defn- det-2x2
  "Finds the determinant of a 2x2 matrix."
  [[[a b] [c d]]]
  (- (* a d)
     (* b c)))

(defn dimensions
  [m]
  [(count m) (count (first m))])

(defn square?
  [m]
  (apply == (dimensions m)))

(defn identity-sign-mat
  [r c]
  (for [x (take r (cycle' [1 -1] [-1 1]))]
    (take c x)))

(defn mat-map
  ([f m]
   (map #(map f %) m))
  ([f m1 m2]
   (map #(map f %1 %2) m1 m2)))

(defn print-mat
  [m]
  (domap println m))

(defn transpose
  [m]
  (apply map list m))

(defn add
  [m1 m2]
  (mat-map + m1 m2))

(defn sub
  [m1 m2]
  (mat-map - m1 m2))

(defn mul
  [m1 m2]
  (let [[a _] (dimensions m1)
        [_ b] (dimensions m2)]
    (if (== a b)
      (for [a m1]
        (for [b (transpose m2)]
          (reduce + (map * a b))))
      (throw (Error. "Illegal dimensions for multiplication.")))))

(defn minors
  [m]
  (for [r (range (count m))
        c (range (count (first m)))]
    (->> m
         (drop-at r)
         (map #(drop-at c %)))))

(defn det
  [m]
  (if (square? m)
    ((fn self [m] (if (= (dimensions m) [2 2])
                    (det-2x2 m)
                    (reduce + (map * (first m)
                                     (cycle [1 -1])
                                     (map self (minors m)))))) m)
    (throw (Error. "Can not find the determinant of a non-square matrix."))))

(defn cofactors
  [m]
  (let [[r c] (dimensions m)
        dets (->> m minors (map det) (partition r))]
    (mat-map * dets (identity-sign-mat r c))))

(defn adjoint
  [m]
  (-> m cofactors transpose))

(defn inverse
  [m]
  (let [d (det m)]
    (if (not (zero? d))
      (mat-map #(* % (/ d)) (adjoint m))
      (throw (Error. "Non-invertible matrix.")))))

;;
;; tests
;;

(defn mat-test
  [x]
  (mul x (inverse x)))

;; these two cases should fail
(mat-test [[1 2 3] [4 5 6] [7 8 9]])
(mat-test [[1] [2 3 4] [5 6 7]])

;; these should return the identity matrix
(print-mat (mat-test [[1 2 3 4] [4 5 6 1] [7 8 -9 3] [1 1 1 1]]))
(print-mat (mat-test [[1 2 3] [4 5 6] [7 8 -9]]))

