(ns semantic-similarity.core)
(use '[clojure.java.shell :only [sh]])
(use '[clojure.string :only [split]])

(def letters #{\a,\b,\c,\d,\e,\f,\g,\h,\i,\j,\k,\l,\m,\n,\o,\p,\q,\r,\s,\t,\u,\v,\w,\x,\y,\z,
               \A,\B,\C,\D,\E,\F,\G,\H,\I,\J,\K,\L,\M,\N,\O,\P,\Q,\R,\S,\T,\U,\V,\W,\X,\Y,\Z})


(defn print-n-return [thing]
  (println thing)
  thing)

(defn get-only-alpha [my-str]
  (apply str (filter #(letters %) my-str)))

(defn get-part-of-speech [word]
  (reduce #(if (second %2)        ;part of speech is true  
             (conj %1 (first %2)) ;add the letter to the list
             %1)                  ;just return the list
          [] 
   (let [response (:out (sh "wn" word))]
    (into {} (map #(vector (first %) 
                  (if (.contains response (str "Information available for " (second %)))
                    true
                    false))
      {"n" "noun" "v" "verb" "a" "adj" "r" "adv"})))))

(defn get-word-trees [word]
  (map #(:out (sh "wn" word (str "-hype" %))) 
       (get-part-of-speech word))) 

(defn determine-levels [synonym-string]
  (if (not (.contains synonym-string "=>"))
    0
    (let [indent-count (.indexOf synonym-string "=>")]
      (/ (- indent-count 3) 4)))) 
 

(defn handle-a-level [state level-string]
  (let [level-num (determine-levels level-string) id (+ (state :id) 1)] 
  {
   :id
   id

   :cur-path
   (if (> (count (state :cur-path)) level-num)
     (conj (subvec (state :cur-path) 0 level-num) id) 
     (conj (state :cur-path) id))

   :sense 
   (conj (state :sense)
    {:id id 
     :parent (last (butlast (state :cur-path)))
     :data (map ;;this will return a level broken into a vector of synonym maps
      (fn [split-synonym] 
        (hash-map 
          :word (get-only-alpha split-synonym)
          :level level-num)) 
      (split level-string #","))})}))


(defn split-str-into-senses [word-tree-string]
  (rest (apply concat (map #(split %1 #"Sense") word-tree-string))))

(defn children-from-parent [sense]
  (reduce 
    (fn [child-struct level] (if (level :parent) 
                               (assoc child-struct 
                                    (level :parent) 
                                    (conj (child-struct (level :parent)) (level :id))) 
                               child-struct ))
     (into (sorted-map) (do (map (fn [level] [(level :id)[]])
      sense))) 
    sense))

(defn depth-from-children-struct [children-struct]
  (loop [depth-struct (sorted-map)
         index (- (count children-struct) 1)]
    ;(println index)
    (let [new-depth-struct
          (if (> (count (children-struct index)) 0)
            (assoc depth-struct index (+ 1 
                                         (if (> (count (children-struct index)) 1) 
                                         (reduce 
                                            #(if 
                                                (> (depth-struct %1) (depth-struct %2))
                                                (depth-struct %1)
                                                (depth-struct %2))
                                              (children-struct index))
                                         (print-n-return (depth-struct (first (children-struct index)))))))
            (assoc depth-struct index 1))]
      ;(println new-depth-struct)
      (if (= 0 index)
        new-depth-struct  ;terminating condition 
        (recur new-depth-struct (- index 1))))))

(defn insert-depths [strange-data-structure]
  (let [sense (strange-data-structure :sense)]
    (println "Depth-from-children-struct")
    (println (depth-from-children-struct
                (print-n-return (children-from-parent sense))))
    strange-data-structure
    ))

(defn handle-a-sense [sense]
  (let [split-levels (rest (split sense #"\n"))] 
    (->> split-levels
        (reduce handle-a-level 
                {:id -1 :cur-path[] :sense []})
        (insert-depths) 
        )))

(defn tree-to-level-map [word-tree-string]
  (flatten 
    (map 
      handle-a-sense 
      (split-str-into-senses word-tree-string)))) 

(defn collect-duplicates [word-tree]
  (into {} (filter #(if (= "" (first %1)) false true)
    (reduce 
      #(if (contains? %1 (%2 :word))
          (if (< (%1 (%2 :word)) (%2 :level))
            (assoc %1 (%2 :word) (%2 :level))
            %1) 
        (assoc %1 (%2 :word) (%2 :level)))
      {}
      word-tree))))

(defn get-common-ancestors [tree1 tree2]
  (map 
    #(let [word (first %1)]
        {word (+ (tree1 word) (tree2 word))})
    (filter 
      #(if (contains? tree2 (first %1))
        true
        false)
      tree1)))

(defn test-semantics [word1 word2]
  (let [
    
    tree1
    
    (-> word1 
    (get-word-trees)
    (tree-to-level-map)
    (collect-duplicates))

    tree2

    (-> word2 
    (get-word-trees)
    (tree-to-level-map)
    (collect-duplicates))] 

    (get-common-ancestors tree1 tree2))) 

(defn test-handle-a-sense []
       (-> "dog"
          (get-word-trees)
          (tree-to-level-map)))
       



