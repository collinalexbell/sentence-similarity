(ns semantic-similarity.core)
(use '[clojure.java.shell :only [sh]])
(use '[clojure.string :only [split]])

(def letters #{\a,\b,\c,\d,\e,\f,\g,\h,\i,\j,\k,\l,\m,\n,\o,\p,\q,\r,\s,\t,\u,\v,\w,\x,\y,\z,
               \A,\B,\C,\D,\E,\F,\G,\H,\I,\J,\K,\L,\M,\N,\O,\P,\Q,\R,\S,\T,\U,\V,\W,\X,\Y,\Z})


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
 

(defn parse-word-trees [word-trees]
    (flatten (map 
      #(map 
        (fn [synonyms] 
          (let [level (determine-levels synonyms)] 
          (map
            (fn [split-synonym] 
              (hash-map 
                (get-only-alpha split-synonym)
                level)) 
            (split synonyms #","))))
            (rest (split % #"\n")))
    ;discard the first part
    (rest
      ;break up the word trees by Sense 
      (apply concat (map #(split %1 #"Sense") word-trees)))))) 


(defn test-semantics []
  (-> dog
    (parse-word-trees)))




