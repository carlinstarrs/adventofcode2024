library("tidyverse")

input <- readLines("inputs/day08_input.txt")

ncols <- nchar(input[1])
nrows <- length(input)

gridded <- input %>%
  paste0(collapse = "") %>% 
  stringr::str_split(pattern = '') %>% 
  unlist() %>% 
  matrix(nrow = nrows,
         ncol = ncols,
         byrow = TRUE) 

nodes <- c(gridded) %>% 
  unique() %>% 
  .[!. %in% c(".", "#")]

node_locs <- map(nodes, \(x) which(gridded == x, arr.ind = TRUE)) %>% 
  setNames(nodes)

get_antinodes <- function(n1, n2, ncols, nrows, part){ 
  if(part == "p1"){
    this_dist <- n1 - n2
    d1 <- n1 + this_dist
    d2 <- n2 - this_dist
  } else {
    d1 <- antinode_p2(n1, n1 - n2, ncols, nrows)
    d2 <- antinode_p2(n2, (n1 - n2) * -1, ncols, nrows)
    
  }
  antinodes <- rbind(d1, d2)
}

antinode_p2 <- function(node, dist, ncols, nrows){ 
  pass <- TRUE
  i <- 0
  d1 <- matrix(ncol = 2)
  while(pass){
    next_step <- node - (dist * i)
    pass <- check_antinode(next_step, ncols, nrows)
    d1 <- rbind(d1, next_step)
    i <- i + 1 
  }
  return(d1)
}

get_all_antinodes <- function(node, ncols, nrows, part = "p1"){
  ids <- 1:nrow(node)
  all_locs <- combn(ids, m = 2)
  antinodes <- list()
  for(i in 1:ncol(all_locs)){
    locs <- node[all_locs[,i],]
      antinodes <- c(antinodes, 
                     list(get_antinodes(locs[1,], locs[2,], ncols, nrows, part)))
  }
  
  #filter to valid antinodes
  all_antinodes <- do.call(rbind, antinodes) 
  
  fil <- which(all_antinodes[,2] <= ncols & 
                 all_antinodes[,1] <= nrows & 
                 all_antinodes[,1] > 0 & 
                 all_antinodes[,2] > 0)
  
  all_antinodes <- all_antinodes[fil,]
  
  return(all_antinodes)
}

check_antinode <- function(pos, ncols, nrows){
  fil <- which(pos[2] <= ncols & 
                 pos[1] <= nrows & 
                 pos[1] > 0 & 
                 pos[2] > 0)
  
  length(fil) > 0
}

map(node_locs, \(x) get_all_antinodes(x, ncols, nrows, part = "p1")) %>%
  do.call(rbind, .) %>%
  unique() %>%
  as.data.frame() %>%
  arrange(row, col) %>%
  nrow()

map(node_locs, \(x) get_all_antinodes(x, ncols, nrows, part = "p2")) %>% 
  do.call(rbind, .) %>% 
  unique() %>% 
  as.data.frame() %>% 
  arrange(row, col) %>% 
  nrow()


