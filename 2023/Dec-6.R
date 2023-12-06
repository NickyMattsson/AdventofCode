library(stringi)
library(tidyverse)

###### Dec. 6
### Part 1
times = c(44,82,69,81)
records = c(202,1076,1138,1458)

result = 1
for (i in 1:length(times)){
  possibilities = 1:times[i]
  tmp = (possibilities *  (times[i] - possibilities) > records[i]) %>% 
    sum()
  result = result * tmp
}


### Part 2
times = 44826981
records = 202107611381458

possibilities = 1:times
tmp = (possibilities *  (times - possibilities) > records) %>% 
  sum()