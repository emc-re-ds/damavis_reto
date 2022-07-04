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
  
  for(i in c(-1,1))
  {
    # row movement
    if(i + snake[[1]][1] >= 0 && i + snake[[1]][1] <= board[1])
    {
      if(list(c(i + snake[[1]][1], snake[[1]][2])) %in% snake == FALSE)
      {
        possible_cells = append(possible_cells, list(c(i + snake[[1]][1], snake[[1]][2])))
      }
    }
    
    # column movement
    if(i + snake[[1]][2] >= 0 && i + snake[[1]][2] <= board[2])
    {
      if(list(c(snake[[1]][1], i + snake[[1]][2])) %in% snake == FALSE)
      {
        possible_cells = append(possible_cells, list(c(snake[[1]][1], i + snake[[1]][2])))
      }
    }
  }
  
  return(possible_cells)
}

numberOfAvailableDifferentPaths = function(board, snake, depth)
{
  result = 0
  
  if(depth == 0)
  {
    return(1)
  }
  else
  {
    # print(snake)
    allowed = allowedCells(snake, board)
    for(c in allowed)
    {
      # print(allowedCells(snake, board))
      result = result + numberOfAvailableDifferentPaths(board, updateSnake(snake, c), depth-1) 
    }
  }
  
  return(result)
}

#### TEST 1
board = c(4,3) # columns x rows
snake = list(c(2,2),c(3,2), c(3,1), c(3,0), c(2,0), c(1,0), c(0,0))
numberOfAvailableDifferentPaths(board, snake, 3)

#### TEST 2
board=c(2,3)
snake= list( c(0,2), c(0,1), c(0,0), c(1,0), c(1,1), c(1,2))
numberOfAvailableDifferentPaths(board, snake, 10)

#### TEST 3
board=c(10,10)
snake= list( c(5,5), c(5,4), c(4,4), c(4,5) )
numberOfAvailableDifferentPaths(board, snake, 4)

