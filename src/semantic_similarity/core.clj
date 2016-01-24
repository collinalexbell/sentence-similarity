(ns semantic-similarity.core)
(require '[clojure.math.numeric-tower :as math])
(use '[clojure.java.shell :only [sh]])
(use '[clojure.string :only [lower-case split]])
(use '[semantic-similarity.vector :only [vec-subtract vec-add vec-norm vec-dot-product cross-product]])

(def letters #{\a,\b,\c,\d,\e,\f,\g,\h,\i,\j,\k,\l,\m,\n,\o,\p,\q,\r,\s,\t,\u,\v,\w,\x,\y,\z,
               \A,\B,\C,\D,\E,\F,\G,\H,\I,\J,\K,\L,\M,\N,\O,\P,\Q,\R,\S,\T,\U,\V,\W,\X,\Y,\Z})

(defn print-seq [seq]
  (doseq [item seq] (println item))
  )

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
      {"n" "noun" "v" "verb"})))))

(defn get-word-trees [word]
  (map #(:out (sh "wn" word (str "-hype" %))) 
       (get-part-of-speech word))) 

(defn determine-levels [synonym-string]
  (if (not (.contains synonym-string "=>"))
    0
    (let [indent-count (.indexOf synonym-string "=>")]
      (/ (- indent-count 3) 4)))) 
 

(defn handle-a-level [state level-string]
  ;(println "<handle-a-level level-string>")
  ;(println level-string)
  ;(println "</handle-a-level level-string>")
  (let [level-num (determine-levels level-string)

        id (+ (state :id) 1)

        cur-path
          (if (> (count (state :cur-path)) level-num)
              (conj (subvec (state :cur-path) 0 level-num) id) 
              (conj (state :cur-path) id))
        ] 
    ;(println "<handle-a-level level-num>")
    ;(println level-num)
    ;(println "</handle-a-level level-num>")

  {
   :id
   id

   :cur-path
   cur-path

   :sense 
   (conj (state :sense)
    {:id id 
     :parent (last (butlast cur-path))
     :data (map ;;this will return a level broken into a vector of synonym maps
      (fn [split-synonym] 
        (hash-map 
          :word (get-only-alpha split-synonym)
          :level level-num)) 
      (split level-string #","))})}))


(defn split-str-into-senses [word-tree-string]
  (rest (apply concat (map #(split %1 #"Sense") word-tree-string))))

(defn children-from-parent [sense]
  ;(println "<Children-from-parent Sense>")
  ;(println sense)
  ;(println "</Children-from-parent Sense>")
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
  ;(println "<Children Struct>")
  ;(println children-struct)
  ;(println "</Children Struct>")
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
                                         (depth-struct (first (children-struct index))))))
            (assoc depth-struct index 1))]
      ;(println new-depth-struct)
      (if (= 0 index)
        new-depth-struct  ;terminating condition 
        (recur new-depth-struct (- index 1))))))

(defn insert-depths [strange-data-structure]
  (let [sense (strange-data-structure :sense)]
    (let [ depth-data (depth-from-children-struct
                (children-from-parent sense))]
      (map (fn [sense-level]
          (let [level-id (sense-level :id)]
             (map (fn [word]
                    (assoc word :depth (depth-data level-id)) )
                  (sense-level :data))))
           (strange-data-structure :sense)))))

(defn handle-a-sense [sense]
  (let [split-levels (rest (split sense #"\n"))] 
    (->> split-levels
        (reduce handle-a-level 
                {:id -1 :cur-path[] :sense []})
        (insert-depths) 
        )))

(defn tree-to-level-map [word-tree-string]
  (filter #(contains? %1 :word) 
    (flatten 
      (map 
        handle-a-sense 
        (split-str-into-senses word-tree-string))))) 

(defn tree-contains [tree word]
  (reduce (fn [item1 item2] 
            (if (or item1 (= (:word item2) word))
              true
              false))
         false
         tree))

(defn write-file [thing-to-write]
  (spit "derp.txt" (str thing-to-write "\n") :append true))

(defn group-by-word [tree]
  (into {} (filter (fn [item] (if (> (count (first item)) 0) true false))
   (reduce 
    (fn [groupings node] 
      (if (contains? groupings (node :word))
         (assoc 
           groupings ;map
           (node :word) ;key
           (conj (groupings (node :word)) node)) ;add node to array
         (assoc 
           groupings ;map again
           (node :word) ;key again
           (list node))))
    {}
    tree))))

(defn get-common-ancestors [tree1 tree2]
  (flatten (let [ 
        grouping1 (group-by-word tree1)
        grouping2 (group-by-word tree2)]
    (map 
      (fn [key]
        (map
          (fn [node1]
            (map 
              (fn [node2]
                (assoc node1 :length (+ (node1 :level) (node2 :level))))
              (grouping2 key)))
          (grouping1 key)))
      (keys grouping1)))))

(def alpha 0.2)
(def beta 0.45)
(def e 2.7182818284590452353602874713527)

(defn depth-score [depth]
  (/ (- (math/expt e (* beta depth)) 
        (math/expt e (* -1 beta depth)))
     (+ (math/expt e (* beta depth))
        (math/expt e (* -1 beta depth)))))

(defn length-score [length]
  (math/expt e (* -1 alpha length)))

(defn make-score [tree]
  (reduce max
   (map (fn [item]
         (* 
           (depth-score (item :depth))
           (length-score (item :length))))
        tree)))

;I NEED TO FIND LENGTH

(defn test-semantics [word1 word2]
  (let [
    
    tree1
    
    (-> word1 
    (get-word-trees)
    (tree-to-level-map))

    tree2

    (-> word2 
    (get-word-trees)
    (tree-to-level-map)) 


    ancestors 
    (get-common-ancestors tree1 tree2)]

    (if (> (count ancestors) 0)
      (make-score ancestors)
      0
      )))

(def semantic-vector-threshold 0.3)

(defn semantic-max [semantic-structs]
  (reduce 
    #(if (> (:score %1) (:score %2))
      %1 
      %2)
  semantic-structs))

(def stop-words 
  (split (slurp "resources/stopwords.txt") #"\n"))

(defn get-half-si-vector [sentence1 sentence2]
  (map
    (fn [word1]
      (list    ;makes a list of (word max-score)
        word1
        (semantic-max   ;gets the max of those scores
          (map ;will return scores of t1i * t2
            (fn [word2]
              ;(println "<word score>")
              ;(println word1)
              ;(println sentence2)
              (let [score (test-semantics word1 word2)]
                ;(println score)
              ;(println "</word score>")
                (if (< (.indexOf stop-words word1) -1)
                  {:score 0 :w2 word2 :sentence-index 1}
                 (if (> (.indexOf sentence2 word1) -1)
                  {:score 1 :w2 word1 :sentence-index 1} 
                  (if (> score semantic-vector-threshold)
                      {:score score :w2 word2 :sentence-index 1}
                      {:score 0 :w2 word2 :sentence-index 1} )))))
               (split sentence2 #" ")))))
    (split sentence1 #" ")))

(defn get-word-counts [si-vector]
  (reduce
    (fn [count-map word-struct]
      (if (contains? count-map (first word-struct))
        (assoc count-map (first word-struct) (+ (count-map (first word-struct)) 1))
        (assoc count-map (first word-struct) 1)))
    {}
    si-vector))


(def tot-words-in-corpus 329794508)

(defn parse-int  [s]
     (Integer.  (re-find  #"\d+" s )))

(def frequency-map
  (into {} (map 
    (fn [word-&-freq]
        (let  [[word frequency] (split word-&-freq #",")]
          [(lower-case word) (parse-int frequency)])
      )
    (split (slurp "resources/word-freq.csv") #"\n"))))
 

(defn get-information-content [word-struct si-vec]

  (let [
        count (frequency-map (first word-struct))
        ]
    (println count)
   (- 1 
     (/
      (Math/log (+ (if count count 0) 1))
      (Math/log (+ tot-words-in-corpus 1))))))

(defn get-joint-word-set [sentence1 sentence2]
  (apply sorted-set (concat 
    (split sentence1 #" ") 
    (split sentence2 #" "))))


(defn pre-process-sentence [sentence]
  (lower-case sentence))

(defn assign-word-order [si-vec joint-word-set]
  (let [ jws-as-vec
        (into [] joint-word-set)]
  (map 
    #(list 
       (first %1)
       (assoc 
          (second %1)
          :word-order
          (.indexOf jws-as-vec (first %1))))
    si-vec)))

(defn assign-information-content-weight [si-vec]
   (map 
     (fn [item]
       (list
         (first item)
         (assoc (second item) :weight (get-information-content item si-vec))))
      si-vec))

(defn si-to-set [si-vector joint-word-set]
  ;we will count the number of times words occur 
  (into [] (reduce 
    #(if (contains? %1 (first %2))
         (assoc %1 (first %2) 
            (assoc (second %2) :sentence-index 3))
         (assoc %1 (first %2) (second %2)))
    {}
    si-vector)))

(defn get-full-si-vector [sentence1 sentence2 joint-word-set]
  (-> (concat
    (map 
     (fn [word]
       (list word {:score 1 :w2 word :sentence-index 0})) 
     (split sentence1 #" "))
    (get-half-si-vector sentence2 sentence1))
  (assign-information-content-weight)
  (assign-word-order joint-word-set)
  (si-to-set joint-word-set))) 
 
(defn filter-by-sentence [si-vec sentence-index]
  (map
    #(if (or (= sentence-index ((second %1) :sentence-index))
             (= 3 ((second %1) :sentence-index)))
       %1 
       (list 
         (first %1)
         (assoc (second %1) :score 0 :word-order 0)))
    si-vec))

(defmacro extract-from-si-vec [key-to-extract si-vec]
 `(map
    #(~key-to-extract (second %1))
    ~si-vec))

(defn map-si-vec-to-sentence [sentence si-vec]
  ;(println "<sentence>")
  ;(println sentence)
  ;(println "</sentence>")
  (let [si-map (into {} si-vec )]
  (map #(do
         (list %1 
              (si-map %1)))
    (split sentence #" "))))

(defn order-score [sentence0 si-vec0 sentence1 si-vec1]
  (let 
    [r0
     (extract-from-si-vec :word-order 
        (map-si-vec-to-sentence sentence0 si-vec0))
     ;(filter-by-sentence si-vec 0) 

     r1
     (extract-from-si-vec :word-order 
        (map-si-vec-to-sentence sentence1 si-vec1))
     ]
    (- 1 
       (/ (vec-norm (vec-subtract r0 r1))
       (vec-norm (vec-add r0 r1))))))

(defn semantic-score [si-vec0 si-vec1]
  (let [
    scores0 (extract-from-si-vec :score si-vec0)

    scores1 (extract-from-si-vec :score si-vec1) 

    weights0 (extract-from-si-vec :weight si-vec0)

    weights1 (extract-from-si-vec :weight si-vec1)
    
    s0 (cross-product scores0 weights0)
    s1 (cross-product scores1 weights1)]
    
  (/ (vec-dot-product s0 s1)
     (* (vec-norm s0) (vec-norm s1)))))

(def semantic-over-order 0.7);blast off, (you gotta have fun right?)

(defn <-by-order [item1 item2]
  (if (< 
    (:word-order (second item1)) 
    (:word-order (second item2)))
    true
    false)) 

(defn get-sentence-similarity [sentence1 sentence2]
  (let [ 
     sentences
     [(pre-process-sentence sentence1)
      (pre-process-sentence sentence2)]

     joint-word-set
     (get-joint-word-set (first sentences) (second sentences))

     si-vec0
     (sort <-by-order
      (get-full-si-vector 
        (first sentences) (second sentences) joint-word-set))

     si-vec1
     (sort <-by-order
      (get-full-si-vector 
        (second sentences) (first sentences) joint-word-set))] 


    (+ 
      (* (- 1 semantic-over-order) (order-score 
                                     (first sentences) si-vec0 
                                     (second sentences) si-vec1))
      (* semantic-over-order (semantic-score si-vec0 si-vec1)))))

