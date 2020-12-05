library(tidyverse)

bin2dec <- function(x)
{
  b <- stringr::str_split(x, pattern = "") %>% unlist()
  pow <- 2 ^ ((length(b) - 1):0)
  sum(pow[b == 1])
}

dd <- read_lines("test")

d1 <- str_replace_all(dd, "F", "0") %>% 
  str_replace_all("B", "1") %>% 
  str_extract("[0-1]{7}")

d2 <- str_replace_all(dd, "L", "0") %>% 
  str_replace_all("R", "1") %>% 
  str_extract("[0-1]{3}")

d1 <- as.integer(d1)
d2 <- as.integer(d2)
rows <- sapply(d1, bin2dec)
cols <- sapply(d2, bin2dec)
max(rows * 8 + cols) #first star!

sort(rows*8 + cols)[which(diff(sort(rows * 8 + cols)) > 1)] + 1 #second star!








