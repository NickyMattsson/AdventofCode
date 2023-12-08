library(stringi)
library(tidyverse)

###### 7. December
# data

directions = "LRLRRRLRRLRLLRRRLRRLLRRRLLRLRRLLRRRLRLRRRLRRLRRLRRLLLLRRRLRRLRRRLRRRLRRRLRLRRRLRLLRRRLLRLRRRLRRRLRLRRLLRLLRRLRLLRRRLLRRLRLLLRLRLRLLRRRLRLRLRRLRRRLRRLRLRRRLRRRLRRRLLLLRLLRRLLRRRLRRLRRLLRRLRRRLLRRLLLRRRLRLRLLRRLRRRLRRLRRRLLRLRRRLRLLRLLRRRLRRLLRLRRRLRRLRRRLRRLRRLRRRLRRLRRRR" %>% 
  str_split(pattern = "") %>% unlist()
len_dir = length(directions)

data = readLines("input8.txt") %>% 
  gsub(pattern = "=",x=.,replacement = ",") %>% 
  gsub(pattern = "\\(",x=.,replacement = "") %>%
  gsub(pattern = " ",x=.,replacement = "") %>% 
  gsub(pattern = "\\)",x=.,replacement = "") %>% 
  read.table(text = .,sep=",") %>% 
  arrange(V1)

colnames(data) = c("From","L","R")


#### Del 1
i = 1
m = 0
From = "AAA" %>% data.frame(From=.)

repeat{
  From = From %>% 
    left_join(data,by=join_by(From == From)) %>% 
    reframe(From = !!sym(directions[i]))
  
  if(From$From == "ZZZ" || ((m*len_dir + i)>1e6)){
    break
  }
  
  i = i+1
  
  if (i > len_dir){
    i=1
    m = m+1
    print(m)
  }
} 

#### Del 2
i = 1
m = 0
idx = substr(data$From,3,3)=="A"
From = data$From[idx] %>% data.frame(From=.)

iters = rep(0,nrow(From))
repeat{
  From = From %>% 
    left_join(data,by=join_by(From == From)) %>% 
    reframe(From = !!sym(directions[i]))
  
  idx = substr(From$From,3,3) == "Z"
  if(any(idx)){
    iters[idx]=m*len_dir + i
  }
  
  if(all(iters>0) || ((m*len_dir + i)>1e6)){
    break
  }
  
  i = i+1
  
  if (i > len_dir){
    i=1
    m = m+1
    print(m)
  }
} 
options(scipen=999) #Ellers kan man ikke se tallet...
out = Reduce(Lcm, iters)
