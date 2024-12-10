library("tidyverse")
options(scipen = 999)

input <- readLines("inputs/day09_input.txt")
# input <- readLines("inputs/jk9input.txt")
input <- c("2333133121414131402")
input <- c("12345")

parse_disk_map <- function(disk_map){
  files <- disk_map[seq(1,length(disk_map),2)]
  free <- disk_map[seq(2,length(disk_map),2)]
  
  if(length(files) < length(free)) files <- c(files, 0)
  if(length(free) < length(files)) free <- c(free, 0)
  
  i <- 0
  map2(files, free, function(x, y){
    out <- c(rep(i, x), rep(".", y))
    i <<- i + 1
    return(out)
  }) 
}

move_blocks <- function(disk_map){
  j <- length(disk_map)
  for(i in 1:length(disk_map)){
    if(i > j) break
    to_replace <- which(disk_map[[i]] == ".")
    if(length(to_replace) == 0) next
    while(length(to_replace) > length(disk_map[[j]][disk_map[[j]] != "."])){
      j <- j - 1
      disk_map[[j]] <- c(disk_map[[j]],disk_map[[j+1]])
      disk_map[[j+1]] <- NULL
    }
    disk_map[[j]] <- disk_map[[j]][disk_map[[j]] != "."]
    replacement <- rev(tail(disk_map[[j]], length(to_replace)))
    disk_map[[i]][which(disk_map[[i]] == ".")] <- replacement
    
    if(i != j){
      disk_map[[j]] <- disk_map[[j]][-((length(disk_map[[j]])-length(replacement)+1):length(disk_map[[j]]))]
    }
    
    if(i >= length(disk_map)) break
    
  }
  
  tibble(disk_map = disk_map %>% unlist() %>% as.numeric(),
         id = 0:(length(disk_map)-1)) %>% 
    mutate(checksum = disk_map*id) %>% 
    pull(checksum) %>% 
    sum()
  
}

str_split(input, "") %>% 
  unlist() %>% 
  parse_disk_map() %>% 
  move_blocks()

move_blocks2 <- function(disk_map){
    void_lengths <- map(disk_map, \(x) str_extract_all(x, "\\.") %>% unlist() %>% length())
    file_lengths <- map(disk_map, \(x) x[x != "." & x == x[1]] %>% unlist() %>% length())
    
    for(j in length(disk_map):1){
      print(j)
      how_long_is_this_file <- file_lengths[[j]]
      void_to_fill <- which(void_lengths >= how_long_is_this_file)
      void_to_fill <- void_to_fill[void_to_fill < j][1]
      voids <- which(disk_map[[void_to_fill]] == ".")
      if(length(voids) == 0) next
      replacement <- disk_map[[j]][disk_map[[j]] != "." & disk_map[[j]] == disk_map[[j]][1]]
      disk_map[[void_to_fill]][voids[1:length(replacement)]] <- replacement
      disk_map[[j]][disk_map[[j]] == replacement] <- rep(".", length(replacement))
      
      void_lengths <- map(disk_map, \(x) str_extract_all(x, "\\.") %>% unlist() %>% length())
      
    }
    
  })
  
  tibble(disk_map = disk_map %>% unlist() %>% as.numeric(),
         id = 0:(length(disk_map)-1)) %>% 
    mutate(checksum = disk_map*id) %>% 
    pull(checksum) %>% 
    sum(na.rm = TRUE)
  
}

str_split(input, "") %>% 
  unlist() %>% 
  parse_disk_map() %>%  
  move_blocks2()


 


