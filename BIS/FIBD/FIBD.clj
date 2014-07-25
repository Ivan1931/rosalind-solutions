(defn fib
  [a]
  (defn fib-iter
    [y z iter]
    (cond 
      (> 0N iter) 0N
      (>= 1N iter) z
      :else (fib-iter z (+ z y) (- iter 1))))
  (fib-iter 1N 1N a))

(defn rabbit-breeder-killer
  [n m]
  (let [p (- n 2)
        q (- m 2)])
  (- (fib p) (fib (- p q))))

