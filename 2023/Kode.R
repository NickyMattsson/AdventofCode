library(stringi)
library(tidyverse)
data = read.csv("input.txt",header=FALSE) %>% pull()

first = stringi::stri_extract_first_regex(data, "[0-9]")
last = stringi::stri_extract_last_regex(data, "[0-9]") 
total = paste0(first,last) %>% as.numeric() %>% sum()



numbers_verb = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
numbers_num = 1:9
translation = data.frame(num = numbers_num, verb = numbers_verb)

data_trans = data #init
for (j in length(data)){
  min_idx = Inf
  min_idx_num = 0
  
  for (i in 1:nrow(translation)){
    idx = grep(x = data_trans[10],pattern=translation$verb[1])
    if (idx<min_idx){
      min_idx=idx
      min_idx_num =i
    }
  }
  data_trans[j] = gsub(x = data_trans[j],pattern=translation$verb[min_idx_num],replacement = translation$num[min_idx_num])
}

first = stringi::stri_extract_first_regex(data_trans_left, "[0-9]")
last = stringi::stri_extract_last_regex(data_trans_right, "[0-9]") 
total = paste0(first,last) %>% as.numeric() %>% sum()




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
