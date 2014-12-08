require 'set'
require 'pry'


require_relative 'DEG'

def get_neighbours graph, vertex
  neighbours = []
  graph[vertex].each_with_index { |node, i| neighbours.push i if node != 0 }
  return neighbours
end

def depth_first_search graph, vertex
  to_visit = [vertex]
  visited = Array.new graph.length, false
  current = vertex
  while current
    current = to_visit.pop
    if current
      visited[current] = true
      to_visit += (get_neighbours graph, current).select { |node| !visited[node] } #adds all unvisited connected nodes to list of things to visit
    end
  end
  visited
end

def visited_to_indexes visited
  indices = []
  visited.each_with_index { |vertex, i| indices.push i if vertex }
  indices
end

def connected_components graph
  components = Set.new
  known = Set.new
  len = graph.length
  components.add(Set.new(visited_to_indexes(depth_first_search graph, 0)))
  (1..len - 1).each do |vertex|
    unless known.include? vertex
      nodes = Set.new(visited_to_indexes(depth_first_search graph, vertex))
      unless components.include? nodes
        components.add nodes
        known.merge nodes
      end
    end
  end
  components
end

def num_connected_components graph
  (connected_components graph).length
end
