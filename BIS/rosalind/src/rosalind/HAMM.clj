
(use '[clojure.java.io]
     '[clojure.string :as string :only [lower-case split-lines]])

(def nums (mapv (partial + 3) (range 1 20)))

(apply / nums)
