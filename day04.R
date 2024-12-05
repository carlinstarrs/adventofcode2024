library("tidyverse")

input <- readLines("inputs/day04_input.txt")

# 
# input <- c("MMMSXXMASM",
#            "MSAMXMSMSA",
#            "AMXSXMAAMM",
#            "MSAMASMSMX",
#            "XMASAMXAMM",
#            "XXAMMXXAMA",
#            "SMSMSASXSS",
#            "SAXAMASAAA",
#            "MAMMMXMMMM",
#            "MXMXAXMASX")

ncols <- nchar(input[1])
nrows <- length(input)

gridded <- input %>%
  paste0(collapse = "") %>% 
  stringr::str_split(pattern = '') %>% 
  unlist() %>% 
  matrix(nrow = nrows, 
         ncol = ncols, 
         byrow = TRUE)

#part 1
get_neighbors <- function(x, y, dir = NA){
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  out <- matrix(
    c(
      x + 1, y, "E",
      x - 1, y, "W",
      x, y + 1, "S",
      x, y - 1,  "N",
      x + 1, y - 1, "NE",
      x - 1, y - 1, "NW",
      x + 1, y + 1, "SE",
      x - 1, y + 1, "SW"
    ), 
    nrow = 8, 
    ncol = 3, 
    byrow = TRUE,
    dimnames = list(NULL, c("col", "row", "dir"))
  )
  
  out <- subset(out, out[,2] >= 1 & out[,1] >= 1)
  
  if(!is.na(dir)){
    out <- subset(out, out[,3] == dir)
  }
  
  return(out)
}

magic_word <- "XMAS" %>%
  str_split("") %>% 
  unlist()

positions <- magic_word %>% 
  imap(\(val, i){
    pos <- which(gridded == val, arr.ind = T) 
    pos %>% 
      cbind(matrix(rep(i, nrow(pos)), nrow = nrow(pos), dimnames = list(NULL, "val"))) %>% 
      cbind(matrix(rep(NA, nrow(pos)), nrow = nrow(pos), dimnames = list(NULL, "dir"))) %>% 
      .[, c("col", "row", "val", "dir")]
  }) 

queue <- subset(positions[[1]], positions[[1]][,1] %in% c(6) & positions[[1]][,2] %in% c(5))
queue <- positions[[1]]
found_words <- matrix(nrow = 1, ncol = 3, dimnames = list(NULL, c("row", "col", "val")))

while(length(queue) > 1){
    x <- queue[1, "col"]
    y <- queue[1, "row"]
    val <- as.numeric(queue[1, "val"])
    dir <- queue[1, "dir"]
    
    if(val < 4){
      valid_neighbors <- get_neighbors(x, y, dir)
      matches <- merge(valid_neighbors, positions[[val + 1]][,-4])
      if(nrow(matches) > 0){
        queue <- rbind(queue, matches)
      }
    } else if (val == 4){
      found_words <- found_words %>% rbind(matrix(c(x, y, val), nrow = 1, ncol = 3, dimnames = list(NULL, c("col", "row", "val"))))
    }
  
  queue <- queue[-1,]
}

nrow(found_words) - 1

#part 2
get_mas_neighbors <- function(x, y, dir = NA){
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  out <- matrix(
    c(
      # x + 1, y, "E",
      # x - 1, y, "W",
      # x, y + 1, "S",
      # x, y - 1,  "N",
      x + 1, y - 1, "NE",
      x - 1, y - 1, "NW",
      x + 1, y + 1, "SE",
      x - 1, y + 1, "SW"
    ), 
    nrow = 4, 
    ncol = 3, 
    byrow = TRUE,
    dimnames = list(NULL, c("col", "row", "dir"))
  )
  
  out <- subset(out, out[,2] >= 1 & out[,1] >= 1)
  
  if(!is.na(dir)){
    out <- subset(out, out[,3] == dir)
  }
  
  return(out)
}

magic_word <- "MAS" %>%
  str_split("") %>% 
  unlist()

positions <- magic_word %>% 
  imap(\(val, i){
    pos <- which(gridded == val, arr.ind = T)
    if(i == 2){
      ids <- 1:nrow(pos)
    } else {
      ids <- rep(NA, nrow(pos))
    }
    pos %>% 
      cbind(matrix(rep(i, nrow(pos)), nrow = nrow(pos), dimnames = list(NULL, "val"))) %>% 
      cbind(matrix(rep(NA, nrow(pos)), nrow = nrow(pos), dimnames = list(NULL, "dir"))) %>% 
      cbind(matrix(ids, nrow = nrow(pos), dimnames = list(NULL, "id"))) %>% 
      .[, c("id", "col", "row", "val", "dir")]
  }) 

#queue <- subset(positions[[1]], positions[[1]][,2] %in% c(2) & positions[[1]][,3] %in% c(1))
queue <- positions[[1]]
found_words <- matrix(nrow = 1, ncol = 4, dimnames = list(NULL, c("id", "row", "col", "val")))

while(length(queue) > 1){
  x <- queue[1, "col"]
  y <- queue[1, "row"]
  val <- as.numeric(queue[1, "val"])
  dir <- queue[1, "dir"]
  id <- queue[1, "id"]
  
  if(val < 3){
    valid_neighbors <- get_mas_neighbors(x, y, dir)
    matches <- merge(valid_neighbors, positions[[val + 1]][,c(-5)])
    if(nrow(matches) > 0){
      if(!is.na(id) & all(is.na(matches$id))){
        matches <- matches[,-4]
        matches <- matches %>% 
          cbind(matrix(id, nrow = nrow(matches), dimnames = list(NULL, "id"))) %>% 
          .[c("id", "col", "row", "val", "dir")]
      }

      queue <- rbind(queue, matches)
    }
  } else if (val == 3){
    found_words <- found_words %>% 
      rbind(matrix(c(id, x, y, val), nrow = 1, ncol = 4, dimnames = list(NULL, c("id", "col", "row", "val"))))
  }
  
  queue <- queue[-1,]
}

found_words
out <- table(found_words[,1])
out <- out[out == 2]
length(out)
