library("tidyverse")
input <- readLines("inputs/day03_input.txt")

mul <- function(x,y){
 as.numeric(x)*as.numeric(y)
}

#part 1 
stringr::str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)") %>% 
  unlist() %>% 
  map(\(x) eval(parse(text = x))) %>% 
  unlist() %>% 
  sum()

#part 2
parsed <- stringr::str_extract_all(input, "(do(|n't)\\(\\))|(mul\\(\\d{1,3},\\d{1,3}\\))") %>% 
  unlist() 

do <- TRUE 
out <- c()
walk(parsed, function(x){
  if(x == "don't()"){
    do <<- FALSE
  } else if(x == "do()"){
    do <<- TRUE
  }

  if(do){
    out <<- c(out, eval(parse(text = x)))
  } 
}) 

sum(out)

