(ns utils)

(defn domap
  "For when you wish doseq looked more like map.
  => (domap println [1 2 3 4])"
  ([f x]
   (doseq [i x] (f i)))
  ([f x & more]
   (doseq [i (apply map list x more)]
     (apply f i))))

(defn drop-at
  "Drops the nth element of sequence m."
  [n m]
  (concat (take (int n) m) (drop (inc (int n)) m)))

(defn cycle'
  "Calls cycle on the result of mapping cycle to each of its arguments.
   In other words, a lazy and infinite list of lazy infinite lists.
   => (cycle' [1] [0])
   ((1 1 1 ...)
    (0 0 0 ...)
    (1 1 1 ...)
    ...)"
  [& more]
  (cycle (map cycle more)))


