library("tidyverse")
library("bit64")
options(scipen = 999)
input <- readLines("inputs/day07_input.txt")

input <- input %>% 
  map(\(x){
    str_split(x, pattern = ":| ") %>% 
      unlist() %>% 
      as.numeric() %>% 
      .[!is.na(.)]
  })

guess_and_check <- function(cal, operators){
  result <- cal[1]
  vals <- cal[-1]

  iters <- gtools::permutations(n = length(operators), r = length(vals)-1, v=operators, repeats.allowed = TRUE)
  #matrix(iters[6,], ncol = 3, byrow = TRUE)
  success <- FALSE
  j <- 1

  while(j <= nrow(iters) & success == FALSE){
    x <- iters[j,]
    running <- vals[1]
    
    for(i in 2:length(vals)){
      running <- eval(parse(text = paste0(running, x[i-1],vals[i])))
      if(running > result) break
    }
    
    if(running == result){
      success <- TRUE
    }
    
    j <- j + 1
  }
  
  if(success){
    return(result)
  } else {
    return(0)
  }
}

future::plan(future::multisession)

part1 <- furrr::future_imap(input, function(x, i){
  guess_and_check(x, operators = c("*", "+"))
}) 

part1 %>% 
  unlist() %>% 
  sum()

out <- furrr::future_imap(input, function(x, i){
  guess_and_check(x, operators = c("*", "+", ""))
}) 

