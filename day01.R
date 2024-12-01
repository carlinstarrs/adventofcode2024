library("tidyverse")

#part 1
input <- read.table("inputs/day01_input.txt")
y1 <- setNames(input$V1, 1:length(input$V1))
y2 <- setNames(input$V2, 1:length(input$V2))

dists <- c()

for(i in 1:length(y1)){ 
  # I should have just used sort here but I thought it was asking 
  # the distance between the positions not the values
  pos.min.y1 <- as.numeric(which.min(y1))
  pos.min.y2 <- as.numeric(which.min(y2))

  dists[i] <- abs(y1[pos.min.y1] - y2[pos.min.y2])
  
  y1[pos.min.y1] <- NA
  y2[pos.min.y2] <- NA
}

sum(dists)

#part 2
similarities <- c()

for(i in 1:length(y1)){
  appearances <- length(y2[y2 == y1[i]])
  
  similarities[i] <- y1[i] * appearances
}

sum(similarities)
