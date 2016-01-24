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



