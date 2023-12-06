library(stringi)
library(tidyverse)

###### Dec. 4
##### Del 1
data = read.table(text = gsub(":", "|", readLines("input4.txt")),sep = "|")
Winning_numbers = read.table(text = data$V2)
my_numbers = read.table(text = data$V3)

gain = 0
for (i in 1:nrow(Winning_numbers)){
  matches = sum(my_numbers[i,] %in%  Winning_numbers[i,])
  gain = gain + ifelse(matches>0,2^(matches-1),0)
}
### Del 2
data = read.table(text = gsub(":", "|", readLines("input4.txt")),sep = "|")
Winning_numbers = read.table(text = data$V2)
my_numbers = read.table(text = data$V3)

matches=c()
for (i in 1:nrow(Winning_numbers)){
  matches[i] = sum(my_numbers[i,] %in%  Winning_numbers[i,])
}

scratchcards = rep(1,nrow(Winning_numbers))
for (i in 1:length(matches)){
  max_match = min(i+matches[i],length(matches))
  if(max_match>=i+1){
    scratchcards[(i+1):max_match] = scratchcards[(i+1):max_match] + scratchcards[i]
  }
}

sum(scratchcards)

