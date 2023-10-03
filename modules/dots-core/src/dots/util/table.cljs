(ns dots.util.table
  "Map of maps, tracking insertion order.")

(defn tassoc [table k m]
  (let [order (get-in table [k :order] (count table))]
    (assoc table k (assoc m :order order))))

(defn tupdate [table k f & args]
  (if-let [m (get table k)]
    (assoc table k (apply f m args))
    (tassoc table k (apply f nil args))))

(defn tvals [table]
  (sort-by :order (vals table)))
