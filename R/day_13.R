library(tidyverse)

timestamp <- 1007268
dd <- "17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,937,x,x,x,x,x,x,x,x,
x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,397,x,x,x,x,x,37,
x,x,x,x,x,x,x,x,x,x,x,x,19"

bus_times <- str_remove_all(dd, "x,") %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()

wait_times <- bus_times - (timestamp %% bus_times)
best_bus <- which.min(wait_times)
bus_times[best_bus] * wait_times[best_bus] #first star!



test_times <- str_split(dd,",") %>% 
  unlist() %>% 
  str_detect("[0-9]") %>% 
  which() 

library(bit64)
gotcha <- as.integer64(1)
jump <- as.integer64(1)
working_on <- 1
timestamp <- as.integer64(1)

#'
#' Once we know that a bus 1 comes at the right time, we only have to check timestamp + 17 from there on out.
#' 
#' Similarly, if we know bus 1 and 2 come at the right time, we only have to check timestamp + 17 * 41 from there on.
#' 
#' This makes it run way faster!
#'

repeat {
  bus_wait <- timestamp %% bus_times
  person_wait <- bus_times - bus_wait
  person_wait[person_wait == 0] <- bus_wait[person_wait == 0]
  if(all((test_times - person_wait) %% bus_times == 0)) {
    print(timestamp)
    break
  }
  if((test_times[working_on] - person_wait[working_on]) %% bus_times[working_on] == 0) {
    gotcha <- c(gotcha, bus_times[working_on])
    jump <- prod(gotcha)
    working_on <- working_on + 1
    print(jump)
  }
  timestamp <- timestamp + jump
}
timestamp + 1 #second star!