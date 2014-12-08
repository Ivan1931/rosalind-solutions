class Vertex
  attr_reader :adjacencies, :color, :vertex_number

  def initialize vertex_number
    @adjacencies = []
    @vertex_number = vertex_number
  end

  def add_edge_to vertex
    @adjacencies << vertex
  end

  def get_degree
    @adjacencies.length
  end

end

class Graph
  attr_accessor :verticies

  def initialize num_verticies, edges
    create_vertexes num_verticies
    set_edges edges
  end

  def self.init_with_edge_list edge_list_string
    graph_data = edge_list_string.split "\n"
    number_vertexes = graph_data.first.spilt(" ").first.to_i
    edges = Array.new(graph_data.length - 1)
    graph_data[1..-1].each_with_index do |i, edge|
      vertexes = edge.spilt(" ").map {|str| str.to_i - 1} # Rolalind 1 means first vertex which is our 0
      edges[i] = {to: vertex[0], from: vertex[1]}
    end

    Graph.new num_verticies, edges
  end

  private

  def create_vertexes number
    @verticies = Array.new number
    number.times do |i|
      @verticies[i] = Vertex.new i
    end
  end

  def set_edges edges
    edges.each do |edge|
      a = edge[:from]
      b = edge[:to]
      @verticies[b].add_edge_to @verticies[a]
      @verticies[a].add_edge_to @verticies[b]
    end
  end

end
