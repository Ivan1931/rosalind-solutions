def parent_index(n)
  ((n + 1) / 2).to_i - 1
end

# This will bubble up the first empty element in the list
def bubble_up(list, bottom_right_idx)
  current_idx = bottom_right_idx
  #keep swapping the parent and current value until we reach a parent that is greater than the current
  loop do
    parent_idx = parent_index current_idx
    parent = list[parent_idx]
    current = list[current_idx]
    if parent_idx >= 0 and parent < current
      list[current_idx], list[parent_idx] = list[parent_idx], list[current_idx]
      parent_idx, current_idx = current_idx, parent_idx
    else 
      break
    end
  end
  list
end

def build_heap(list)
  heap = Array.new list.length
  list.each_with_index do |elem, i|
    heap[i] = elem
    heap = bubble_up heap, i
  end
  heap
end
