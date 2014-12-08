require 'pry'


def mk_digraph num_vertexes, edge_strings
  graph = Array.new(num_vertexes) { [] }
  edge_strings.each do |edge|
    temp = edge.split " "
    from = temp[0].to_i - 1
    to = temp[1].to_i - 1
    graph[from] << to
  end
  graph
end

def get_digraph_neighbours digraph, vertex
  digraph[vertex]
end


BIG_NUMBER = 1_000_000

def breadth_first_search digraph, vertex
  queue = [vertex]
  dist = Array.new digraph.length, BIG_NUMBER
  dist[vertex] = 0

  until queue.empty?
    current = queue.shift
    get_digraph_neighbours(digraph, current).each do |forthcoming|
      if dist[forthcoming] == BIG_NUMBER
        queue << forthcoming
        dist[forthcoming] = dist[current] + 1
      end
    end
  end

  dist.map { |i| if i == BIG_NUMBER then -1 else i end }
end

def mk_graph_from_file path
  contents = File.read path
  lines = contents.split "\n"
  num_vertexes = lines.first.split(" ").first.to_i
  mk_digraph num_vertexes, lines[1..-1]
end
