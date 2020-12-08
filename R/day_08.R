library(tidyverse)
library(igraph)

dd <- read_lines("data/day_08")

dd <- data.frame(x = dd)

dd <- dd %>% 
  separate(col = x, into = c("command", "value"), sep = " ", convert = TRUE) %>% 
  mutate(visits = 0, row = 1:nrow(dd))

dd$visits <- 0
current <- 1
acc <- 0
while(all(dd$visits < 2)) {
  if(dd$visits[current] == 1) break
  dd$visits[current] <- dd$visits[current] + 1
  if(dd$command[current] == "acc") {
    acc <- acc + dd$value[current]
    current <- current + 1
  }
  if(dd$command[current] == "nop") {
    current <- current + 1
  }
  if(dd$command[current] == "jmp") {
    current <- dd$value[current] + current
  }
}
acc #first star!

dd[627,] <- list(command = "stp", value = 0, visits = -1, row = 627)
vv <- which(dd$command %in% c("nop", "jmp"))

for(v in vv) {
  dd$command[v] <- ifelse(dd$command[v] == "nop", "jmp", "nop")
  dd$visits[1:626] <- 0
  current <- 1
  acc <- 0
  while(all(dd$visits < 2)) {
    if(dd$visits[current] == 1 || dd$visits[current] == -1) break
    dd$visits[current] <- dd$visits[current] + 1
    if(dd$command[current] == "acc") {
      acc <- acc + dd$value[current]
      current <- current + 1
    }
    if(dd$command[current] == "nop") {
      current <- current + 1
    }
    if(dd$command[current] == "jmp") {
      current <- dd$value[current] + current
    }
  }
  if(dd$visits[current] == -1) {
    break
  } else{
    dd$command[v] <- ifelse(dd$command[v] == "nop", "jmp", "nop")
  }
}
acc #second star!
