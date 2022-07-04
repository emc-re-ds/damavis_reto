# move snake across the board
updateSnake = function(snake, cell)
{
  cell_aux = snake[[1]]
  snake[[1]] = cell
  
  for(i in 2:length(snake))
  {
    cell_aux2 = snake[[i]]  
    snake[[i]] = cell_aux
    cell_aux = cell_aux2
  }
  
  return(snake)
}

# all possible cells where the snake is allowed to go
allowedCells = function(snake, board)
{
  possible_cells = list()
  
  # the movements happen simultaneously
  # so the head could take the place of the tail
  for(i in c(-1,1))
  {
    # row movement
    row_move = i + snake[[1]][1]
    if(row_move >= 0 && row_move < board[1])
    {
      # check if there is self-intersection
      if(list(c(row_move, snake[[1]][2])) %in% snake[1:length(snake)-1] == FALSE)
      {
        possible_cells = append(possible_cells, list(c(row_move, snake[[1]][2])))
      }
    }
    
    # column movement
    col_move = i + snake[[1]][2] 
    if(col_move >= 0 && col_move < board[2])
    {
      # check if there is self-intersection
      if(list(c(snake[[1]][1], col_move)) %in% snake[1:length(snake)-1] == FALSE)
      {
        possible_cells = append(possible_cells, list(c(snake[[1]][1], col_move)))
      }
    }
  }
  
  return(possible_cells)
}

# using a recursive-backtracking approach to explore all possible solutions 
numberOfAvailableDifferentPaths = function(board, snake, depth)
{
  result = 0
  
  # if there are no more movements to do, then the path has reach the end and
  # return back to check more paths
  if(depth == 0)
  {
    return(1)
  }
  else
  {
    # for each available cell where the snake can move into, the 
    # recursive call will be made.
    allowed_cells = allowedCells(snake, board)
    for(c in allowed_cells)
    {
      result = (result + numberOfAvailableDifferentPaths(board, updateSnake(snake, c), depth-1))%%(1000000007)
    }
  }
  
  return(result)
}

#### TEST 1
board = c(4,3)
snake = list( c(2,2), c(3,2), c(3,1), c(3,0), c(2,0), c(1,0), c(0,0) )
test1 = numberOfAvailableDifferentPaths(board, snake, 3)

#### TEST 2
board = c(2,3)
snake = list( c(0,2), c(0,1), c(0,0), c(1,0), c(1,1), c(1,2) )
test2 = numberOfAvailableDifferentPaths(board, snake, 10)

#### TEST 3
board = c(10,10)
snake = list( c(5,5), c(5,4), c(4,4), c(4,5) )
test3 = numberOfAvailableDifferentPaths(board, snake, 4)

print(c(test1, test2, test3))

#### ADITIONAL TESTS
board = c(2,6)
snake = list( c(0,2), c(0,1), c(0,0) ) 
test4 = numberOfAvailableDifferentPaths(board, snake, 2)

board = c(2,4)
snake = list( c(0,2), c(0,1), c(0,0), c(1,0), c(1,1), c(1,2) ) 
test5 = numberOfAvailableDifferentPaths(board, snake, 10)
