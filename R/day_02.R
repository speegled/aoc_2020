library(tidyverse)

dd <- read.csv("data/day_02.csv", header = FALSE)
dd <- dd$V1

v1 <- str_extract(dd, "^[0-9]+") %>% as.integer()
v2 <- str_remove(dd, "^[0-9]+-") %>% 
  str_extract("^[0-9]+") %>% as.integer()

v3 <- str_remove(dd, "^[0-9]+-") %>%
  str_remove("^[0-9]+ ") %>% 
  str_extract("^[a-z]")

v4 <- str_remove(dd, "^[0-9]+-") %>%
  str_remove("^[0-9]+ ") %>% 
  str_remove("^[a-z]: ")

sapply(1:length(v4), function(x) {
  str_count(v4[x], v3[x]) <=  v2[x] && str_count(v4[x], v3[x]) >=  v1[x]
}) %>% sum() #first star

sapply(1:length(v4), function(x) {
  sum(str_sub(v4[x], start = c(v1[x], v2[x]), end = c(v1[x], v2[x])) == v3[x]) == 1
}) %>% sum() #second star
