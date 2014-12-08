require_relative 'HEA'
require 'pry'

# convert n to 1 indexed number and subtract one so that we are again at a zero indexed number
def left_child_index(n)
  (n + 1) * 2 - 1
end

# convert n to 1 indexed number and we are done
def right_child_index(n)
  (n + 1) * 2
end

# replaces first element with last element and then shifts first element down until the heap property is satisfied
def sift_down(list)
  list[0] = list.last
  list.delete_at(-1)
  heap_unsatisfied = true
  idx = 0
  while heap_unsatisfied
    l_idx = left_child_index(idx)
    r_idx = right_child_index(idx)
    if list[l_idx] and list[r_idx]
      if list[idx] < list[r_idx] or list[idx] < list[l_idx]
        if list[r_idx] < list[l_idx]
          list[idx], list[l_idx] = list[l_idx], list[idx]
          idx = l_idx
        else
          list[idx], list[r_idx] = list[r_idx], list[idx]
          idx = r_idx
        end
      else
        heap_unsatisfied = false
      end
    elsif list[l_idx] and !list[r_idx]
      if list[idx] < list[l_idx]
        list[idx], list[l_idx] = list[l_idx], list[idx]
        idx = l_idx
      end
      heap_unsatisfied = false
    elsif list[r_idx] and !list[l_idx]
      if list[idx] < list[r_idx]
        list[idx], list[r_idx] = list[r_idx], list[idx]
        idx = r_idx
      end
      heap_unsatisfied = false
    else
      heap_unsatisfied = false
    end
  end
  list
end

def heap_sort(list)
  heap = build_heap list
  sorted = Array.new heap.length
  sorted.length.times do |i|
    sorted[i] = heap.first
    heap = sift_down heap 
  end
  sorted
end

file = File.open(ARGV.first, "r")

lines = file.read.split("\n")

numbers = lines[1].split(" ").map {|i| i.to_i }

acc = ""
heap_sort(numbers).reverse.each do |i|
  acc += "#{i} "
end

puts acc

file.close
=begin
a = Array.new 10

r = Random.new
a.length.times do |i|
  a[i] = (r.rand * 1000).to_i
end

b = a.dup

c = heap_sort(b).reverse

a.sort.each_with_index do |elem, i|
  puts "index #{i}:\t#{elem}\t#{c[i]}"
end
=end
