library("tidyverse")
input <- readLines("inputs/day10_input.txt")

ncols <- nchar(input[1])
nrows <- length(input)

gridded <- input %>%
  paste0(collapse = "") %>% 
  stringr::str_split(pattern = '') %>% 
  unlist() %>% 
  as.numeric() %>% 
  matrix(nrow = nrows,
         ncol = ncols,
         byrow = TRUE) 

trailheads <- which(gridded == 0, arr.ind = TRUE)


get_next <- function(x, y, val, full_grid){
  coords <- list(
    c(x, y + 1), 
    c(x, y - 1), 
    c(x - 1, y), 
    c(x + 1, y)
  )
  
  vals <- map(coords, \(x) {
    if(x[2] <= nrow(full_grid) & x[2] > 0 & x[1] <= ncol(full_grid) & x[1] > 0){
      out <- full_grid[x[2], x[1]]
    } else {
      out <- NA
    }
    return(out)
  }) 
  
  vals <- which(vals == val + 1)
  
  if(length(vals) == 0) return(NULL)
  
  out <- do.call(rbind, coords[vals]) %>% 
    cbind(val = val + 1) 
  out <- out[,c("row", "col", "val")]
  
}

travel <- function(trailhead, gridded, part){
  queue <- matrix(c(trailhead, 0), ncol = 3, dimnames = list(NULL, c("row", "col", "val")))
  finished <- 0
  while(nrow(queue) > 0){
    if(part == 1){
      queue <- unique(queue) 
    }
    trail <- queue[1,] 
    
    if(trail[3] == 9){
      finished <- finished + 1
      queue <- matrix(queue[-1,], ncol = 3, dimnames = list(NULL, c("row", "col", "val")))
    } else {
      next_steps <- get_next(trail[2], trail[1], gridded[trail[1], trail[2]], gridded)
      queue <- do.call(rbind, list(queue[-1,], next_steps))
    }
  }
  
  return(finished)
}


map((1:nrow(trailheads)), \(i){
  travel(trailheads[i,], gridded, 1)
})%>% 
  unlist() %>% 
  sum()

map((1:nrow(trailheads)), \(i){
  travel(trailheads[i,], gridded, 2)
})%>% 
  unlist() %>% 
  sum()

