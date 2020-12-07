library(tidyverse)
library(reshape2)
dd <- read_file("data/day_06")

ee <- dd %>%
  strsplit('\n\n')  %>% 
  unlist() %>% 
  strsplit('[ \n]') %>% 
  melt() 




ee %>% 
  group_by(L1) %>% 
  summarize(all = paste(value, collapse = "")) %>% 
  group_by(L1) %>% 
  summarize(n = str_split(all, "") %>% unlist() %>% unique() %>% paste(collapse= "")) %>% 
  group_by(L1) %>% 
  summarize(n = str_length(n)) %>% 
  pull(n) %>% 
  sum #first star!


count_vals <- function(x) {
  aa <- x %>% 
    str_split("") %>%
    melt()
  bb <- length(unique(aa$L1))
  aa %>% 
    group_by(value) %>% 
    summarize(val = n() == bb) %>% 
    pull(val) %>% sum
}

ee %>% 
  group_by(L1) %>% 
  summarize(n = count_vals(value)) %>% 
  pull(n) %>% sum #second star!

#'
#' Re-do using the same idea but with separate_rows
#'


dd <- read_file("data/day_06")

ee <- dd %>%
  strsplit('\n\n')  %>% 
  unlist() %>% 
  strsplit('[ \n]') %>% 
  melt() %>% 
  group_by(L1) %>% 
  mutate(num = n()) %>% 
  separate_rows(value, sep = "") %>% 
  filter(value != "")

ee %>% 
  group_by(L1) %>% 
  summarize(n = length(unique(value))) %>% 
  summarize(sum(n)) #first star

ee %>% 
  group_by(value, L1) %>% 
  summarize(n = n(), num = num) %>%  
  filter(n == num) %>% 
  distinct(L1) %>% 
  nrow() #second star
  




