(ns semantic-similarity.vector)
(require '[clojure.math.numeric-tower :as math])

(defn vec-subtract  [vec1 vec2]
  (map
    #(- %1 %2)
    vec1
    vec2
    ))

(defn vec-add [vec1 vec2]
  (map 
    #(+ %1 %2)
    vec1
    vec2))

(defn vec-norm [vec]
  (math/sqrt
    (reduce
      +
      (map
        #(math/expt %1 2)
        vec))))

(defn vec-normalize [vec]
  (map #(/ %1 (vec-norm vec))
    vec))


(defn vec-dot-product [vec1 vec2]
  (apply + (map #(* %1 %2)
       vec1
       vec2)))


(defn cross-product [vec1 vec2]
  ;pretend that this vec is not transposed
  (map #(* %1 %2) vec1 vec2))
