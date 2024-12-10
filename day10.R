# score is the number of 9-height positions reachable from that trailhead via a hiking trail
input <- readLines("inputs/day10_input.txt")
input <- c(
  "89010123",
  "78121874",
  "87430965",
  "96549874",
  "45678903",
  "32019012",
  "01329801",
  "10456732"
)
# 
# input <- c(
#   "...0...",
#   "...1...",
#   "...2...",
#   "6543456",
#   "7.....7",
#   "8.....8",
#   "9.....9"
# )
# 
# input <- c(
#   "..90..9",
#  "...1.98",
#   "...2..7",
#   "6543456",
#   "765.987",
#   "876....",
#   "987...."
# )
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

travel <- function(trailhead, gridded){
  queue <- matrix(c(trailhead, 0), ncol = 3, dimnames = list(NULL, c("row", "col", "val")))
  finished <- 0
  while(nrow(queue) > 0){
    #queue <- unique(queue) #comment out for part 1 lol
    
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
  travel(trailheads[i,], gridded)
})%>% 
  unlist() %>% 
  sum()


