# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.
library("tidyverse")

test_level <- function(level){
  parsed_level <- as.numeric(unlist(strsplit(level, split = " ")))
  all_inc <- all(map2_lgl(head(parsed_level, -1), tail(parsed_level, -1),  \(x, y) x > y))
  all_desc <- all(map2_lgl(head(parsed_level, -1), tail(parsed_level, -1),  \(x, y) x < y))
  all_btwn <- all(map2_lgl(head(parsed_level, -1), tail(parsed_level, -1),  \(x, y) between(abs(x-y), 1, 3)))
  
  check <- (all_inc == TRUE | all_desc == TRUE) & all_btwn == TRUE
}  

#part 1
map_lgl(example, test_level) %>% sum()
map_lgl(readLines("inputs/day02_input.txt"), test_level) %>% sum()

#part 2
readLines("inputs/day02_input.txt") %>% 
  map_lgl(function(level){
    does_this_work <- test_level(level)
    
    if(does_this_work) return(TRUE)
    
    parsed_level <- unlist(strsplit(level, split = " "))
    i <- 1
    
    while(!does_this_work & i <= length(parsed_level)){
      does_this_work <- test_level(parsed_level[-i])
      i <- i + 1
    }
    
    return(does_this_work)
  }) %>% sum()
