def mk_graph num_vertexes, edge_strings
  graph = Array.new(num_vertexes) { Array.new(num_vertexes, 0) }
  edge_strings.each do |str|
    vals = str.split " "
    x = vals.first.to_i
    y = vals[1].to_i
    graph[x - 1][y - 1] = 1
    graph[y - 1][x - 1] = 1
  end
  graph
end

def degree graph, vertex
  graph[vertex].map {|e| (e != 0) ? 1 : 0 }.reduce(:+)
end

def degree_string graph
  acc = ""
  graph.length.times do |i|
    acc += "#{degree graph, i} "
  end
  acc.chomp
end

def from_el_file file_name
  graph_strings = File.read file_name
  graph_strings = graph_strings.split "\n"
  num_vertexes = graph_strings.first.split(" ").first.to_i
  mk_graph num_vertexes, graph_strings[1..-1]
end
