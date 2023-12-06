library(stringi)
library(tidyverse)

###### 1. December

#### Del 1
data = read.csv("input.txt",header=FALSE) %>% pull()

first = stringi::stri_extract_first_regex(data, "[0-9]")
last = stringi::stri_extract_last_regex(data, "[0-9]") 
total = paste0(first,last) %>% as.numeric() %>% sum()

#### Del 2. FML

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




