# variable declaration
board = c(3,4) # rows x columns
snake = list(c(2,2), 
          c(2,3), 
          c(1,3),
          c(0,3),
          c(0,2),
          c(0,1),
          c(0,0))

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
    if(i + snake[[1]][1] >= 0 && i + snake[[1]][1] < board[1])
    {
      if(list(c(i + snake[[1]][1], snake[[1]][2])) %in% snake == FALSE)
      {
        possible_cells = append(possible_cells, list(c(i + snake[[1]][1], snake[[1]][2])))
      }
    }
    
    # column movement
    if(i + snake[[1]][2] >= 0 && i + snake[[1]][2] < board[2])
    {
      if(list(c(snake[[1]][1], i + snake[[1]][2])) %in% snake == FALSE)
      {
        possible_cells = append(possible_cells, list(c(snake[[1]][1], i + snake[[1]][2])))
      }
    }
  }
  
  return(possible_cells)
}

# numberOfAvailableDifferentPaths = function(board, snake, depth)
# {
# 
# }