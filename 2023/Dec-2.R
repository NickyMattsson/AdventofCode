library(stringi)
library(tidyverse)

###### 2. December
#Data
data = read.csv("input2.txt",header=FALSE,sep = ":")
data = data$V2

#Hjælper funktion
ny_max = function(x){max(as.numeric(x))}

#### Del 1
red = data %>% 
  str_extract_all(pattern="(\\d+)(?=\\s*red)") %>% 
  sapply(ny_max) <= 12


green = data %>% 
  str_extract_all(pattern="(\\d+)(?=\\s*green)")%>% 
  sapply(ny_max) <= 13


blue = data %>% 
  str_extract_all(pattern="(\\d+)(?=\\s*blue)")%>% 
  sapply(ny_max) <=14

result = (red & green & blue) %>% 
  which() %>% 
  sum()

#### Del 2
red = data %>% 
  str_extract_all(pattern="(\\d+)(?=\\s*red)") %>% 
  sapply(ny_max)

green = data %>% 
  str_extract_all(pattern="(\\d+)(?=\\s*green)")%>% 
  sapply(ny_max) 

blue = data %>% 
  str_extract_all(pattern="(\\d+)(?=\\s*blue)")%>% 
  sapply(ny_max)

result = (red * green * blue) %>% 
  sum()