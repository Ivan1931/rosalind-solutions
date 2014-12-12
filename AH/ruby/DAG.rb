require_relative 'graph'

include Algo
include Maybe

def is_cyclic?(graph)
  #first do a depth first search
  graph.dfs.each do |results|
    # Linear time thing to get the depth of each node in the tree
    path = results[:visit_order]
    tree = results[:tree]
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
    graph.vertexes.each do |k, vertex|
      if tree_depths[k]
        vertex.neighbours.each do |neighbour|
          if tree_depths[neighbour.key] and tree_depths[k] > tree_depths[neighbour.key]
            # Now we quickly check if our neighbour is also an ancestor
            # To do this we traverse backwards up the tree and check if we can find a neighbour
            current = k
            until current.nil?
              current = tree[current][:parent]
              if current === neighbour.key
                return true
              end 
            end 
          end
        end
      end
    end
  end
  false
end

def do_problem
  file_contents = File.read ARGV.first
  file_contents.split("\n\n")[1..-1].each do |edge_list|
    graph = Graph.new edge_list
    #binding.pry
    puts (is_cyclic?(graph) ? "-1" : "1")
  end
end

def test
  acyclic_1 = %{6 7
1 2
2 3
2 4
4 5
4 6
5 6
6 3}

  cyclic_1 = %{6 7
1 2
2 3
2 4
4 5
5 6
6 3
6 4}
  ac = Graph.new acyclic_1
  cy = Graph.new cyclic_1
  puts ac.is_cyclic?
  puts cy.is_cyclic?
end

test
#do_problem
