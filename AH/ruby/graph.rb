require 'set'
require 'pry'

module Maybe
  def make_maybe(a)
    if a
      Just.new a
    else
      Nothing.new
    end
  end

  class Nothing
    def from_just
      throw ArgumentException "Calling just from a nothing"
    end

    def is_nothing?
      true
    end

    def to_s
      "Nothing"
    end
  end

  class Just
    def initialize(a)
      @data = a
    end

    def from_just
      @data
    end

    def is_nothing?
      false
    end

    def to_s
      "Just #{@data}"
    end
  end
end

module Algo
  include Maybe
  class Graph
    attr_reader :vertexes

    def initialize(edge_list_string)
      @vertexes = {}
      lines = edge_list_string.split "\n"
      numbers = lines.map {|line| line.split " "}
      number_vertexes = numbers[0][0].to_i # first number in the top line provides the number of edges
      (1 .. number_vertexes).each do |i| # We iterate from 1 to number of vertexes
        add_vertex i
      end
      edges = numbers[1..-1] # the rest of the array is the edges description
      edges.each do |edge|
        start_vertex, destination = edge[0].to_i, edge[1].to_i
        cost = edge[2] ? edge[2].to_i : 0 # the cost in a weighted graph will appear as the third element
        add_edge(start_vertex, destination, cost)
      end
    end

    def add_edge(parent_key, destination_key, cost)
      if parent_key === destination_key
        raise ArgumentException "Same parent and destination key: #{parent_key}" 
      end
      vertex = vertex_for_key(parent_key)
      destination = vertex_for_key(destination_key)
      if vertex.is_nothing? or destination.is_nothing? # we return nothing if the edge does not exist
        return Nothing.new
      else
        destination = destination.from_just
      end
      vertex = vertex.from_just
      vertex.add_edge(destination, cost)
    end

    def add_vertex(key)
      vertex = Vertex.new self, key
      if @vertexes[key]
        raise ArgumentException "Vertex with key #{key.to_s} already exists" 
      else
        @vertexes[key] = vertex
      end
    end

    def exists_vertex?(key)
      !@vertexes[key].nil?
    end

    def [](i)
      vertex_for_key(i)
    end

    def vertex_for_key(key)
      make_maybe @vertexes[key]
    end

    # returns a list of paths taken to visit all nodes
    def dfs
      visited = {}
      vertexes.keys.each {|v| visited[v] = false}
      unvisited = find_first_unvisited visited
      search_results = []
      until unvisited.is_nothing?
        #find a depth first search tree and hash containing the order that each node is visited
        dpst = explore(unvisited.from_just)
        if dpst.is_nothing?
          return Nothing.new
        else
          dpst = dpst.from_just
        end
        if search_results.empty?
          search_results.push dpst
        else
          search_results.each_with_index do |result, i|
            tree = dpst[:tree]
            found = false
            result.each do |v|
              if tree[v] and result.length < tree.keys.length
                results[i] = dpst
                found = true
                break
              end
            end
            break if found
          end
        end
        # Mark each point in the path as visited
        dpst[:visit_order].each do |k|
          visited[k] = true
        end
        unvisited = find_first_unvisited visited
      end
      search_results
    end

    # returns nothing if the starting vertex does not exist
    # returns a hash which is essentially a dps tree
    def explore(vertex)
      #binding.pry if vertexes.size == 4
      vertex = self[vertex]
      visited = Set.new
      visit_order = []
      tree = {}
      stack = []
      if vertex.is_nothing?
        return Nothing.new
      else
        vertex = vertex.from_just
      end
      # we will build a depth first search tree with backedges
      # A backedge will occur if a tree is discovered with an edge that is on a higher level in the tree
      stack.push vertex
      visit_order.push vertex.key
      visited.add vertex.key
      tree[vertex.key] = {:children => []}
      until stack.empty?
        candidate = stack.pop
        candidate.neighbours.each do |dest| # iterate through the neighbours of the vertex
          if !visited.member? dest.key # if the destination has not been visited
            visited.add dest.key
            visit_order.push dest.key # add destination to visitation path
            # Set the destinations parent to the candidate
            tree[dest.key] = {:parent => candidate.key, :children => []}
            # Add the destination key to the list of children of the canditate
            tree[candidate.key][:children].push dest.key
            #add the candidate and destination to the top of the stack so that we can return to them later in the algorithm
            stack.push candidate 
            stack.push dest
            break # leave the loop, this only runs once
          end
        end
      end
      return Just.new({ tree: tree, visit_order: visit_order })
    end

    def topological_sort
      # To get the sorted order, we depth first search the graph
      # The Order of the sort is the order in which the nodes are visited under the dfs
      # We we also check if any backedges exist, if they do, we have a cycle and thus it
      ordered_sort = []
      dfs.each do |result|
        path = result[:visit_order]
        tree = result[:tree]
        tree_depths = {} # store the depth of each dpst node hear
        tree_depths[path.first] = 0 # the root has a depth of zero
        stack = [path.first] # add root to stack
        # while we still have element to examine continue to check tree
        until stack.empty?
          parent = stack.pop
          children = tree[parent][:children]
          parent_depth = tree_depths[parent]
          if children
            children.each do |child|
              # child is one lower than its parent
              tree_depths[child] = parent_depth + 1
              # we add all children to the stack because we can and since this is a tree, we will never encounter the parent again unless
              # the algorithm is broken
              stack.push child
            end
          end
        end

        # A graph has a backedge if one of its ancestors is also a neighbour
        vertexes.each do |k, vertex|
          if tree_depths[k]
            vertex.neighbours.each do |neighbour|
              if tree_depths[neighbour.key] and tree_depths[k] > tree_depths[neighbour.key]
                # Now we quickly check if our neighbour is also an ancestor
                # To do this we traverse backwards up the tree and check if we can find a neighbour
                current = k
                until current.nil?
                  current = tree[current][:parent]
                  if current === neighbour.key
                    return Nothing.new
                  end 
                end 
              end
            end
          end
        end
        if ordered_sort.length < path.length
          ordered_sort = path
        end
      end
      Just.new ordered_sort
    end

    def is_cyclic?
      !topological_sort.is_nothing?
    end

    def vertex_keys
      keys = @vertexes.keys.empty?
      if keys
        Nothing.new
      else
        Just.new keys
      end
    end

    private

    def find_first_unvisited(visited)
      visited.each do |k, v|
        if !v
          return Just.new k
        end
      end
      Nothing.new
    end

  end

  class Vertex
    attr_reader :parent, :key, :edges
    def initialize(parent, key)
      @parent, @key, @edges = parent, key, {}
    end

    def add_edge(destination, cost = 0)
      raise ArgumentException "Destination should not be nil" if !destination
      e = Edge.new destination, self, cost
      edges[destination.key] = e
    end

    def neighbours
      @edges.map { |k, edge| edge.destination }
    end

    def destination_keys
      @edges.map { |k, edge| k }
    end

    def has_edges?
      !@edges.empty?
    end

    def to_s
      acc = ""
      @edges.each do |k, edge|
        acc += edge.to_s + "\n"
      end
      "#{@key}\n#{acc}"
    end
  end

  class Edge
    attr_reader :destination, :parent_vertex, :cost
    def initialize(destination, parent_vertex, cost = 0)
      # A few assertions so that we can rely on the fact that the destination is
      raise ArgumentException "destination null" if !destination
      raise ArgumentException "parent_vertex null" if !parent_vertex
      @parent_vertext, @destination, @cost = parent_vertex, destination, cost
    end

    def destination_key
      @destination.key
    end

    def to_s
      "#{@parent_vertext.key} => #{@destination.key}"
    end
  end
end
