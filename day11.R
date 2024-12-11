library("tidyverse")

input <- readLines("inputs/day11_input.txt")

parsed_input <- input %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.numeric() %>% 
  table()

split_stones <- function(val){
  left_stone <- str_sub(val, 1, nchar(val)/2)
  right_stone <- str_sub(val, (nchar(val)/2)+1, nchar(val)) 
  return(c(as.numeric(left_stone), as.numeric(right_stone)))
}


blink <- function(input, times){
  new_blink <- as.list(rep(NA, length(input)))

  #zeros
  zeros <- which(names(input) == 0)
  new_blink[zeros] <- 1
  
  #evens
  even_digits <- which(names(input) > 0 & nchar(names(input)) %% 2 == 0)
  new_blink[even_digits] <- map(names(input)[even_digits], split_stones)
  
  #the rest
  rest <- (1:length(input))[!(1:length(input)) %in% c(zeros, even_digits)]
  new_blink[rest] <- as.numeric(names(input)[rest]) * 2024

  #retable
  new_blink <- map2(new_blink, input, \(x, y) setNames(rep(y, length(x)), x)) %>% 
    unlist() 
  new_blink <- tapply(new_blink, as.numeric(names(new_blink)), sum)

  return(new_blink)
}

do_blinks <- function(to_blink, blinks){
  this_blink <- 1
  while(this_blink <= blinks){
    to_blink <- blink(to_blink)
    this_blink <- this_blink + 1
  }
  
  return(sum(to_blink))
}

parsed_input %>% 
  do_blinks(75) 

