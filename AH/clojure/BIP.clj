(defrecord Edge [to-vertex weight])

(defrecord Vertex [value edges])

(defn add-vertex
  "Takes a graph and returns a new graph with vertex added"
  [g vertex]
  (assoc g :verticies ((:verticies g) conj vertex)))
