require_relative 'graph'

include Algo
include Maybe

def do_problem
  file_contents = File.read ARGV.first
  g = Graph.new file_contents
  path = g.topological_sort
  if !path.is_nothing?
    path.from_just.each do |v|
      print "#{v} "
    end
  end
end

do_problem
