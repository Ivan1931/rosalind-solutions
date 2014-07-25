(use '[clojure.string :as string :only [join split]]
     '[clojure.core.match :only [match]])

(defn parse-fasta
  "This takes a string that is in fasta format, parses it and returns a hask with the label and the string
  Example:
  >Rosalind_0498
  AAATAAA
  would return
  {:lab rosalind-label, :str the-associated-string}"
  [fasta-string]
  (match fasta-string
         "" {}
         _
         (let [split-string
               (string/split fasta-string #"\n")
               fst
               (first split-string)
               lab
               (if (= (first fst) \>)
                 (subs fst 1)
                 fst)
               content-string
               (string/join (rest split-string))]
           {:lab lab, :str content-string})))

(defn parse-rosalind-fasta-strings
  "Takes a file full of rosalind fasta strings and parses them into fasta hashes"
  [parse-string]
(mapv parse-fasta (string/split parse-string #">")))

;;(parse-rosalind-fasta-strings (slurp "resources/test-string.txt"))

(defn has-related-prefix-suffix
  "This function takes two strings and deduces whether one string has a prefix which is also a suffix of the other string
  Example:
  AAAATTTT
  shares a common prefix with
  TTTTCCCC
  which is TTTT"
  [a b]
  )
