library(stringi)
library(tidyverse)

###### 3. December

# data
data = read.csv("input3.txt",header=FALSE)
data = data$V1

# Find tal og deres placering
nums = str_extract_all(data,pattern="\\d+") %>% 
  unlist() %>% 
  as.numeric()

location = str_locate_all(data,"[0-9]+")

start = location %>% 
  sapply(function(x) x[,1]) %>% 
  unlist()

slut = location %>% 
  sapply(function(x) x[,2]) %>% 
  unlist()

line = location %>% 
  seq_along() %>% 
  sapply(function(x) rep(x,nrow(location[[x]])) ) %>% 
  unlist()

numbers = data.frame(number=nums,line=line,start=start, slut=slut)

#### Del 1
grid = data %>% strsplit(split = "") %>% sapply(function(x) x) %>% t()
lim_row = nrow(grid)
lim_col = ncol(grid)
#Undersøg hvert tal
sum = 0
for (i in 1:nrow(numbers)){
  search_length = numbers$slut[i]-numbers$start[i]+3
  lines = c(
    rep(numbers$line[i]-1, search_length),
    numbers$line[i], numbers$line[i],
    rep(numbers$line[i]+1, search_length))
  
  positions = c(
    (numbers$start[i]-1):(numbers$slut[i]+1),
    (numbers$start[i]-1),(numbers$slut[i]+1),
    (numbers$start[i]-1):(numbers$slut[i]+1)
  )
  idx_keep = lines>0 & lines<=lim_row & positions>0 & positions<=lim_col
  lines = lines[idx_keep]
  positions = positions[idx_keep]
  
  if (any(mapply(function(i, j) grid[i,j], lines, positions) != ".")){
    sum = sum + numbers$number[i]
  }
  
}


#### Del 2
grid = data %>% strsplit(split = "") %>% sapply(function(x) x) %>% t()
lim_row = nrow(grid)
lim_col = ncol(grid)
#Undersøg hvert tal
potential_gears = data.frame(number = c(),line = c(),position = c())
for (i in 1:nrow(numbers)){
  search_length = numbers$slut[i]-numbers$start[i]+3
  #Define search grid
  lines = c(
    rep(numbers$line[i]-1, search_length),
    numbers$line[i], numbers$line[i],
    rep(numbers$line[i]+1, search_length))
  
  positions = c(
    (numbers$start[i]-1):(numbers$slut[i]+1),
    (numbers$start[i]-1),(numbers$slut[i]+1),
    (numbers$start[i]-1):(numbers$slut[i]+1)
  )
  #Remove those that are out of bounds
  idx_keep = lines>0 & lines<=lim_row & positions>0 & positions<=lim_col
  lines = lines[idx_keep]
  positions = positions[idx_keep]
  
  #Are there any matches?
  matches= mapply(function(i, j) grid[i,j], lines, positions) != "."
  if (any(matches)){
    #Yes; save it
    line_tmp = lines[matches]
    pos_tmp = positions[matches]
    potential_gears = data.frame(number = numbers$number[i],line = line_tmp,position = pos_tmp) %>% rbind(potential_gears)
  }
  
}

potential_gears = 1:nrow(potential_gears) %>% data.frame(obs=.) %>% 
  cbind(potential_gears)

gears = potential_gears %>% 
  #required overlap
  left_join(potential_gears,join_by(line == line, position == position, obs > obs)) %>%
  #Calculate required number
  filter(!is.na(obs.y)) %>% 
  mutate(gear.num = number.x * number.y) %>% 
  pull(gear.num) %>% 
  sum()
