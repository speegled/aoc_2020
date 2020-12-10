library(tidyverse)
library(bit64)

dd <- as.integer64(read_lines("data/day_09"))

i <- 0
while(dd[i + 26] %in% outer(dd[(i + 1):(25 + i)], dd[(1 + i):(25 + i)], "+")) {
  i <- i + 1
}
dd[i + 26] #first star!

ii <- i
for(i in 1:477) {
  for(j in (i + 1):478) {
    x <- sum(dd[i:j])
    if(x == dd[26 + ii]) {
      print(sum(c(min(dd[i:j]), max(dd[i: j]))))
      break
    } 
  }
} #second star!
