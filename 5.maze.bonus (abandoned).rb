require 'matrix'
require 'pqueue'
require 'colored'

################
# Abandoned in the middle of refactoring; not currently funcitonal, kept for reference
################



# Possible performance improvement: class Path < LinkedList

# Utility math

class Range
  def sum
    if first <= last
      m = first
      n = exclude_end? ? last - 1 : last
      (n*(n+1) - (m-1)*m) / 2
    end
  end
end

class Array
  def forward_difference
    each_cons(2).map{ |x0,x1| x1 - x0 }
  end
end

class Vector
  VON_NEUMANN_MOVES = { Vector[ 0,+1] => 'e',
                        Vector[ 0,-1] => 'w',
                        Vector[+1, 0] => 's',
                        Vector[-1, 0] => 'n'  }

  def von_neumann_neighbours
    VON_NEUMANN_MOVES.keys.map &method(:+)
  end

  def sum
    inject &:+
  end

  def manhattan_distance(other)
    (other - self).map(&:abs).sum
  end

  def covered?(ranges)
    map2(ranges){ |x, range| range.cover? x }.all?
  end
end

class String
  LETTERS = [*('a'..'z'), *('A'..'Z')]
end


# Maze solving classes

class Maze
  attr_reader :start_node, :goal_node, :goal_cost

  def initialize(costs, start_node, goal_node, goal_cost)
    @nodes = Matrix.build(costs.row_size, costs.row_size) { |*coords| Node.new(self, Vector[*coords], costs[*coords]) }
    @start_node = @nodes[*start_node]
    @goal_node  = @nodes[*start_node]
    @goal_cost  = goal_cost
  end

  def fixed_cost_nodes
    [@start_node, @goal_node]
  end

  def bounds
    Vector[(0...@nodes.row_size), (0...@nodes.column_size)]
  end

  def [](*coords)
    get_node[coords]
  end

  def get_node(coords)
    @nodes[*coords]
  end
end

class Node
  attr_reader :cost

  def initialize(maze, coords, cost)
    @maze   = maze
    @coords = coords
    @cost   = cost
  end

  def is_greedy?
    !maze.fixed_cost_nodes.include? self
  end

  def in_bounds?
    @coords.covered? @maze.bounds
  end

  def neighbours
    @coords.von_neumann_neighbours.map(&@maze.method(:get_node)).select &:in_bounds?
  end
end

class Path
  def initialize(maze)
    @maze = maze
    @cost = 0
    @path = []
  end

  def next_greed_cost
    path.length - 1
  end

  def <<(node)
    @cost += node.cost
    @cost += next_greed_cost if node.is_greedy?
    path << node
  end

  def at_goal?
    @cost == @maze.goal_cost && @path.last == @maze.goal_node
  end

  def in_bounds?
    @cost <= @maze.goal_cost
  end

  def descendants
    path.last.neighbours.map ???
  end

  # Still wrong... but I think admissible
  def min_remaining_cost
    (next_greed_cost ... next_greed_cost + path.last.manhattan_distance(@maze.goal_node)).sum + @maze.goal_node.cost
  end

  def min_total_cost
    @cost + min_remaining_cost
  end
end

maze = Maze.new(Matrix[[0,8,1,7,8,8,5,2,9,5,9,5],
                       [8,5,1,1,5,6,9,4,4,5,2,1],
                       [7,2,3,5,2,9,2,6,9,3,9,4],
                       [9,2,5,9,8,9,5,7,7,5,9,6],
                       [2,4,6,7,1,4,2,6,6,2,5,8],
                       [2,8,1,5,3,8,4,9,7,5,2,3],
                       [2,9,3,5,6,7,2,4,9,4,2,5],
                       [6,3,1,7,8,2,3,3,6,7,9,3],
                       [2,5,7,4,2,7,8,5,5,3,5,8],
                       [5,2,9,8,3,6,1,4,9,5,6,3],
                       [4,6,9,8,5,4,9,7,6,4,6,8],
                       [2,7,7,1,9,9,7,3,7,2,2,5]], Vector[ 0, 0], Vector[11,11], 444)



################
# Abandoned in the middle of refactoring; not currently funcitonal, kept for reference
################

class Path
  def candidates
    last.neighbours.map { |node| Path[*self + [node]] }.select(&:in_bounds?)
  end

  def ==(other)
    last == other.last && next_greed_cost == other.next_greed_cost && cost == other.cost
  end

  def <=>(other)
    min_total_cost <=> other.min_total_cost
  end

  def print_maze
    MAZE.each_with_index do |c, x, y|
      puts '' if y.zero?
      if include? Node[x,y]
        print LETTERS[rindex(Node[x,y])].red
      else
        print Node[x,y].cost
      end
    end
    puts ''
    puts "Cost: #{cost}"
    puts "Min Remaining: #{min_remaining_cost}"
    puts "Min Total Cost: #{min_total_cost}"
  end

  def directions
    forward_difference.map{ |move| MOVES[move] }
  end
end


# Ghetto debugging

class Timer
  class Milestone
    attr_reader :value, :time
    def initialize(value)
      @value = value
      @time  = Time.now
    end
  end

  def initialize(&measure)
    @measure    = measure
    @milestones = [Milestone.new(0)]
  end

  def print_milestones(x)
    if @measure[x] > @milestones.last.value
      milestone = Milestone.new(@measure[x]) 
      puts "#{milestone.value} @ #{milestone.time - @milestones.first.time}s (+#{milestone.time - @milestones.last.time}s)"
      @milestones << milestone
    end
  end
end

TIMER = Timer.new(&:min_total_cost)


# Search

def find_path
  paths = PQueue.new([Path[START_NODE]])
  while !paths.top.at_goal?
    paths.top.print_maze                # Debugging
    # TIMER.print_milestones paths.top    # Debugging
    paths.pop.candidates.each do |path|
      paths << path if !paths.include?(path)
    end
  end
  paths.top
end



# Run search

# path = find_path
# puts "----","SOLUTION:"
# path.print_maze
# p path.directions