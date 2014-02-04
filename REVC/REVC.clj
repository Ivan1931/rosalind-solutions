(defn reverse-complement
  "creates the reverse compliment of a dna string"
  [string]
  (map #(case %
          \A \T
          \T \A
          \C \G
          \G \C 
          % ) (reverse string)))

(defn REVC
  [string]
  (reduce str (reverse-complement string)))

(REVC (slurp "rosalind_revc.txt"))
