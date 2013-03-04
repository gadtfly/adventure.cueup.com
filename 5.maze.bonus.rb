require 'matrix'
require 'pqueue'
require 'colored'

# Utility Math

module Enumerable
  def forward_difference
    each_cons(2).map{ |x0,x1| x1 - x0 }
  end
end

class Range
  def sum
    if first <= last
      m = first
      n = exclude_end? ? last - 1 : last
      (n*(n+1) - (m-1)*m) / 2
    end
  end
end

class Vector
  VON_NEUMANN_MOVES = { Vector[ 0,+1] => 'E',
                        Vector[ 0,-1] => 'W',
                        Vector[+1, 0] => 'S',
                        Vector[-1, 0] => 'N'  }

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



# Search logic

class Maze
  
  # Painfully stateful, but faster than immutably recalculating properties from a list
  class Path
    include Enumerable
    attr_reader :location, :came_from, :cost, :turns

    class << self
      def start(location)
        new location, nil, 0, 0
      end
    end
    
    def initialize(location, came_from, cost, turns)
      @location  = location
      @came_from = came_from
      @cost      = cost
      @turns     = turns
    end

    def branch(location, cost)
      Path.new(location, self, @cost + cost, @turns + 1)
    end

    def ==(other)
      @location == other.location && @cost == other.cost && @turns == other.turns
    end

    def each(&f)
      @came_from.each &f if @came_from
      f[self]
    end

    def locations
      map(&:location)
    end

    def directions
      locations.forward_difference.map &Vector::VON_NEUMANN_MOVES.method(:[])
    end
  end


  # Path/Node methods requiring Maze state

  def is_greedy?(location)
    !@fixed_cost.include? location
  end

  #clean up
  def neighbours(location)
    location.von_neumann_neighbours.select { |new_location| new_location.covered? @bounds }
  end

  #clean up
  def move_cost(path, location)
    cost  = @costs[*location]
    cost += path.turns if is_greedy?(location)
    cost
  end

  # Still wrong... but I think admissible
  def min_remaining_cost(path)
    return 0 if @goal_location == path.location
    (path.turns + 1 ... path.turns + path.location.manhattan_distance(@goal_location)).sum + @costs[*@goal_location]
  end

  def min_total_cost(path)
    path.cost + min_remaining_cost(path)
  end

  def path_cmp(path0, path1)
    min_total_cost(path0) <=> min_total_cost(path1)
  end


  # Maze search

  def initialize(costs, start_location, goal_location, goal_cost)
    @costs          = costs
    @start_location = start_location
    @goal_location  = goal_location
    @goal_cost      = goal_cost
    @bounds         = Vector[0...costs.row_size, 0...costs.column_size]
    @fixed_cost     = [start_location, goal_location]
  end

  def at_goal?(path)
    @goal_location == path.location && @goal_cost == path.cost
  end

  # Still wrong... but I think admissible
  def in_bounds?(path)
    path.location.covered?(@bounds) && @goal_cost >= min_total_cost(path)
  end

  #clean up
  def branches(path)
    paths = []
    neighbours(path.location).each do |location|
      new_path = path.branch(location, move_cost(path, location))
      paths << new_path if in_bounds? new_path
    end
    paths
  end

  def find_path
    paths = PQueue.new [Path.start(@start_location)], &method(:path_cmp)
    while !at_goal?(paths.top)
      # print_path(paths.top)   # Debugging
      branches(paths.pop).each do |path|
        paths << path if !paths.include? path
      end
    end
    paths.top
  end


  # User output

  def print_path(path)
    @costs.each_with_index do |c, x, y|
      puts '' if y.zero?
      if path.locations.include? Vector[x,y]
        print String::LETTERS[path.locations.rindex(Vector[x,y])].red
      else
        print c
      end
    end
    puts ''
    puts "Turns: #{path.turns}"
    puts "Distance: #{path.location.manhattan_distance(@goal_location)}"
    puts "Cost: #{path.cost}"
    puts "Min Remaining: #{min_remaining_cost(path)}"
    puts "Min Total Cost: #{min_total_cost(path)}"
  end

  def print_solution
    path = find_path
    puts '----', 'Solution:'
    print_path(path)
    puts path.directions.join ''
  end
end



# Maze description & solution

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

maze.print_solution
# EWEWEWEESSEESSEESSSESEESSSEE