(comment (use '[clojure.string :as string :only (join split)])
;;gc-content file

(defn percentage
  [x]
  (* (double x) 100))

(defn gc-content
  "Computes the gc-content of a given string
  That is the percentage of G and C strings out of the entire string"
  [dna-string]
  (defn filter-char
    [c]
    (filter  (partial = c) dna-string))
  (def count-G
    (count (filter-char \G)))
  (def count-C
    (count (filter-char \C)))
  (def length (count dna-string))
  (percentage
   (/ (+ count-C count-G) length)))

(defn fasta-string
  "Reads a rosalind fasta formatted string and returns the label and string in vector of two elements"
  [fasta]
  (let [split-string
        (string/split fasta #"\n")
        drop-if->
        (fn [s]
          (if (= (first s) \>) (subs s 1) s))
        lab
        (drop-if-> (first split-string))
        gc
        (gc-content (string/join (rest split-string)))]
  [lab gc]))

(defn parse-fasta-strings
  "reads a group of fasta strings and returns a vector of vectors of fasta-strings"
  [fasta-strings]
  (map fasta-string (string/split fasta-strings #"\n>")))

(defn max-fasta
  "Takes a list of unparsed fasta strings and return the fasta label and string with the maximum gc-content"
  [unparsed-fasta-strings]
 (first (reverse (sort-by second (parse-fasta-strings unparsed-fasta-strings)))))

(defn solve-gc-problem
  "Solves the rosalind gc-content problem and prints the output to standard out. Requires a path to the file data"
  [file-path]
(println (string/join "\n" (max-fasta (slurp file-path)))))
  )

;;(solve-gc-problem "resources/rosalind_gc.txt")
