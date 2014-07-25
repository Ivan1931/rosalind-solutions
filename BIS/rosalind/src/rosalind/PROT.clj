(use '[clojure.java.io]
     '[clojure.string :as string :only [lower-case split-lines]])

(def rna-codon-table
 {"UUU" "F" "CUU" "L" "AUU" "I" "GUU" "V" "UUC" "F" "CUC" "L" "AUC" "I" "GUC" "V" "UUA" "L" "CUA" "L" "AUA" "I" "GUA" "V" "UUG" "L" "CUG" "L" "AUG" "M" "GUG" "V" "UCU" "S" "CCU" "P" "ACU" "T" "GCU" "A" "UCC" "S" "CCC" "P" "ACC" "T" "GCC" "A" "UCA" "S" "CCA" "P" "ACA" "T" "GCA" "A" "UCG" "S" "CCG" "P" "ACG" "T" "GCG" "A" "UAU" "Y" "CAU" "H" "AAU" "N" "GAU" "D" "UAC" "Y" "CAC" "H" "AAC" "N" "GAC" "D" "UAA" "" "CAA" "Q" "AAA" "K" "GAA" "E" "UAG" "" "CAG" "Q" "AAG" "K" "GAG" "E" "UGU" "C" "CGU" "R" "AGU" "S" "GGU" "G" "UGC" "C" "CGC" "R" "AGC" "S" "GGC" "G" "UGA" "" "CGA" "R" "AGA" "R" "GGA" "G" "UGG" "W" "CGG" "R" "AGG" "R" "GGG" "G" })

(defn break-to-threes
  [s]
  (map
       (partial apply str)
       (partition 3 s)))

(defn solve-problem
  [s]
  (apply
     str
     (take-while #(not= "" %)
     (map rna-codon-table
          (break-to-threes s)))))

(solve-problem "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")


(def problem-string
  (slurp "resources/rosalind_prot.txt"))

(with-open
  [wrtr (writer "resources/out.txt")]
  (.write wrtr (solve-problem problem-string)))
