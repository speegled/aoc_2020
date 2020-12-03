library(tidyverse)

dd <- read_lines("data/day_03")

dd <- str_replace_all(dd, "\\.", "0") %>% 
  str_replace_all("#", "1")

dd <- str_split(dd, "") %>% 
  unlist()  %>% 
  as.integer() %>% 
  matrix(byrow = T, nrow = 323)

dd2 <- dd
for(i in 1:100) {
  dd2 <- cbind(dd2, dd)  
}
i <- 1
j <- 1

summ <- 0
while(i <= 323) {
  summ <- summ + dd2[i, j]
  i <- i + 1
  j <- j + 3
}

summ #first star!

i <- 1
j <- 1
summ2 <- 0
while(i <= 323) {
  summ2 <- summ2 + dd2[i, j]
  i <- i + 1
  j <- j + 1
}


i <- 1
j <- 1
summ3 <- 0
while(i <= 323) {
  summ3 <- summ3 + dd2[i, j]
  i <- i + 1
  j <- j + 5
}

i <- 1
j <- 1
summ4 <- 0
while(i <= 323) {
  summ4 <- summ4 + dd2[i, j]
  i <- i + 1
  j <- j + 7
}


i <- 1
j <- 1
summ5 <- 0
while(i <= 323) {
  summ5 <- summ5 + dd2[i, j]
  i <- i + 2
  j <- j + 1
}

summ * summ2 * summ3 * summ4 * summ5 #second star!

