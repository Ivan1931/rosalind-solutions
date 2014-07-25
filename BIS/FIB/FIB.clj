(defn rabbit-breeder
  [n k]
  (defn breed-func
    [a b iter]
    (if (= iter 0)
      b
      (breed-func b (+ b (* k a)) (- iter 1))))
  (breed-func 1 1 (- n 2)))
