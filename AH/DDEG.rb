require 'pry'

require_relative 'DEG'


g = from_el_file ARGV[0]

def neighbour_degrees graph
  len = graph.length
  neighbour_degrees = Array.new len
  len.times do |i|
    count = 0
    len.times do |j|
      if j != i and graph[i][j] != 0
        count += degree graph, j
      end
    end
    neighbour_degrees[i] = count
  end
  neighbour_degrees
end

def DDEG graph
  neighbour_degrees(graph).each do |val|
    puts "#{val} "
  end
end

DDEG(g)
