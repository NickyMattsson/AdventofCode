library(stringi)
library(tidyverse)

###### Dec. 5
##### Del 1

#Data
source("input5.R")
seed.to.soil = seed.to.soil %>% mutate(V4 = V2 + V3)
soil.to.fertilizer = soil.to.fertilizer %>% mutate(V4 = V2 + V3)
fertilizer.to.water = fertilizer.to.water %>% mutate(V4 = V2 + V3)
water.to.light = water.to.light %>% mutate(V4 = V2 + V3)
light.to.temperature = light.to.temperature %>% mutate(V4 = V2 + V3)
temperature.to.humidity = temperature.to.humidity %>% mutate(V4 = V2 + V3)
humidity.to.location = humidity.to.location %>% mutate(V4 = V2 + V3)

seeds = seeds %>% data.frame(input=.)

#Funktioner
mapping.par = function(input,map){
  input %>% 
    left_join(map,join_by(input >= V2, input < V4)) %>% 
    reframe(output = ifelse(is.na(V1),input,input-V2+V1)) %>% 
    pull() %>% 
    return()
}


find.loc.par = function(seeds){
  soil = mapping.par(seeds,seed.to.soil) %>% data.frame(input=.)
  fertilizer = mapping.par(soil,soil.to.fertilizer)%>% data.frame(input=.)
  water = mapping.par(fertilizer,fertilizer.to.water)%>% data.frame(input=.)
  light = mapping.par(water,water.to.light)%>% data.frame(input=.)
  temperature = mapping.par(light,light.to.temperature)%>% data.frame(input=.)
  humidity = mapping.par(temperature,temperature.to.humidity)%>% data.frame(input=.)
  location = mapping.par(humidity,humidity.to.location)
  return(location)
}

# Find løsning
seeds %>% find.loc.par() %>% min()


##### Del 2 - Søg baglæns
# Data
source("input5.R")
seeds = seeds %>% data.frame(input=.)

seed.to.soil = seed.to.soil %>% mutate(V4 = V1 + V3)
colnames(seed.to.soil) = c("V2","V1","V3","V4")

soil.to.fertilizer = soil.to.fertilizer %>% mutate(V4 = V1 + V3)
colnames(soil.to.fertilizer) = c("V2","V1","V3","V4")

fertilizer.to.water = fertilizer.to.water %>% mutate(V4 = V1 + V3)
colnames(fertilizer.to.water) = c("V2","V1","V3","V4")

water.to.light = water.to.light %>% mutate(V4 = V1 + V3)
colnames(water.to.light) = c("V2","V1","V3","V4")

light.to.temperature = light.to.temperature %>% mutate(V4 = V1 + V3)
colnames(light.to.temperature) = c("V2","V1","V3","V4")

temperature.to.humidity = temperature.to.humidity %>% mutate(V4 = V1 + V3)
colnames(temperature.to.humidity) = c("V2","V1","V3","V4")

humidity.to.location = humidity.to.location %>% mutate(V4 = V1 + V3)
colnames(humidity.to.location) = c("V2","V1","V3","V4")

#Funktioner

mapping.par.na = function(input,map){
  input %>% 
    left_join(map,join_by(input >= V2, input < V4)) %>% 
    reframe(input = ifelse(is.na(V1),NA,input-V2+V1)) %>% 
    return()
}

find.seeds.par = function(location){
  humidity = mapping.par.na(location,humidity.to.location)
  temperature = mapping.par.na(humidity,temperature.to.humidity)
  light = mapping.par.na(temperature,light.to.temperature)
  water = mapping.par.na(light,water.to.light)
  fertilizer = mapping.par.na(water,fertilizer.to.water)
  soil = mapping.par.na(fertilizer,soil.to.fertilizer)
  seeds = mapping.par.na(soil,seed.to.soil)
  return(seeds)
}

# Undersøg alle locations indtil hit

seeds_mat = seeds %>% pull() %>% matrix(ncol = 2,byrow = TRUE) %>% data.frame() %>% mutate(X3=X1+X2)

start_time <- Sys.time()
for (i in 0:100){
  print(i)
  locations = seq(i*1e7+1,(i+1)*1e7,by=1) %>% data.frame(input=.)
  mulige_seeds = locations %>% find.seeds.par()
  hits = mulige_seeds %>% 
    left_join(seeds_mat,join_by(input >= X1, input < X3))
  
  first.hit = which(!is.na(hits$X1)) %>% min()
  if (first.hit<Inf){
    result = first.hit + i*1e7
    break
  }
}
tid = Sys.time() - start_time