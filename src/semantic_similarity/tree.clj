(ns semantic-similarity.tree)

(defprotocol Tree
  (get-depth [this])
  (add-child [this])
  )

(deftype node [car cdr]
  Tree
  (get-depth [this]
    (loop [depth 1 cur-node this]
      (if (nil? (this :cdr))
      depth
      (recur (+ 1 depth) @(this :cdr))))))



