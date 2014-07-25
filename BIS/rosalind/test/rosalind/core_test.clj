(ns rosalind.core-test
  (:require [clojure.test :refer :all]
            [rosalind.core :refer :all]
            [clojure.repl :refer :all]))

(comment

(def fasta-example-string ">Rosalind_0498
AAATAAA")

(def multi-line-fasta-example-string (str fasta-example-string "\nCC"))


(deftest parse-fasta-test
  (is (=
   (parse-fasta fasta-example-string)
   ({:lab "Rosalind_0498", :str "AAATAAA"}))))

(deftest parse-fasta-multiline-test
  (is (=
   (parse-fasta multi-line-fasta-example-string)
   ({:lab "Rosalind_0498", :str "AAATAAACC"}))))

(run-all-tests))
