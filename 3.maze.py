from numpy.linalg import norm
from numpy import array, array_equal, diff

# Maze description

MAZE = [[8,8,4,4,5],
        [4,9,6,4,8],
        [8,6,4,1,2],
        [4,8,2,6,3],
        [0,6,8,8,4]]

START_NODE  = array([4,0])
GOAL_NODE   = array([0,4])
GOAL_COST   = 35

MOVES = { (-1 , 0): 'n',
          ( 0 ,+1): 'e',
          (+1,  0): 's',
          ( 0, -1): 'w' }


# Grid movement

def recover_path(path):
  return ''.join(MOVES[tuple(d)] for d in diff(path, axis=0))

def grid_neighbours(node):
  return [node + direction for direction in MOVES.keys()]


# Maze costs

def node_cost(node):
  return MAZE[node[0]][node[1]]

def path_cost(path):
  return sum(map(node_cost, path))


# Node predicates

def at_goal_node(node):
  return array_equal(node, GOAL_NODE)

def in_maze(node):
  return node[0] in range(len(MAZE)) and node[1] in range(len(MAZE[0]))


# Search predicates

def at_goal(path):
  return at_goal_node(path[-1]) and path_cost(path) == GOAL_COST

def in_bounds(path):
  return in_maze(path[-1])      and path_cost(path) <= GOAL_COST


# Heuristics

# ... currently performs faster without this

# def grid_distance(start, end):
#   return norm(end - start, 1)

# def min_remaining_cost(path):
#   return path_cost(path) + grid_distance(path[-1], GOAL_NODE)


# Search

# ... a bit of a mess
def search(path = [START_NODE]):
  if at_goal(path):
    return path
  candidates = filter(in_bounds, [path + [node] for node in grid_neighbours(path[-1])])
  hits = filter(None, map(search, candidates))
  if hits:
    return hits[0]
  return None

print recover_path(search())