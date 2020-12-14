library(tidyverse)
library(bit64)

dd <- read_lines("data/day_14")

str_mask <- function(str, mask) {
  str <- str_split(string = str, pattern =  "") %>% unlist()
  mask <- str_split(string = mask, pattern = "") %>% unlist()
  for(i in 1:36) {
    if(mask[i] != "X") {
      str[i] <- mask[i]
    }
  }
  paste(str, collapse = "")
}

bin2dec <- function(x)
{
  b <- stringr::str_split(x, pattern = "") %>% unlist()
  pow <- 2 ^ ((length(b) - 1):0)
  sum(pow[b == 1])
}

vals <- rep(as.integer64(0), 65000) #this is probably a mistake. yep, it's a mistake. see seccond star.

for(i in 1:length(dd)) {
  if(str_detect(dd[i], "mask")) {
    mask <- str_extract(dd[i], "[01X]+$")
  }
  if(str_detect(dd[i], "mem")) {
    str <- str_extract(dd[i], "[0-9]+$") 
    str <- as.bitstring(as.integer64(str)) %>% 
      str_sub(start = -36, end = -1) 
    mem <- str_extract(dd[i], "\\[[0-9]+\\]") %>% 
      str_remove_all("\\[|\\]") %>% 
      as.integer() 
    vals[mem] <- str_mask(str, mask) %>% bin2dec() %>% as.integer64()
  }
}

sum(vals) #first star!



#'
#' Gonna hash it. Wish me luck. Following https://blog.ephorie.de/hash-me-if-you-can
#'

get_hash <- Vectorize(get, vectorize.args = "x")

str_mask <- function(str, mask) {
  str <- str_split(string = str, pattern =  "") %>% unlist()
  mask <- str_split(string = mask, pattern = "") %>% unlist()
  for(i in 1:36) {
    if(mask[i] == "1") {
      str[i] <- mask[i]
    }
    if(mask[i] == "X") {
      str[i] <- "X"
    }
  }
  
  str <- paste(str, collapse = "")
  N <- str_count(str, "X")
  replacements <- matrix(as.character(combinat::hcube(rep(2, N)) - 1), ncol = N, byrow = F)
  
  sapply(1:nrow(replacements), function(i) {
    for(j in 1:N) {
      str <- str_replace(str, "X", replacements[i,j])
    }
    str
  })
}

hash <- new.env(hash = TRUE, parent = emptyenv(), size = 1000L)

for(i in 1:length(dd)) {
  if(str_detect(dd[i], "mask")) {
    mask <- str_extract(dd[i], "[01X]+$")
  }
  if(str_detect(dd[i], "mem")) {
    val <- as.integer(str_extract(dd[i], "[0-9]+$"))
    mem <- str_extract(dd[i], "\\[[0-9]+\\]") %>% 
      str_remove_all("\\[|\\]") 
    mem <- as.bitstring(as.integer64(mem)) %>% 
      str_sub(start = -36, end = -1)
    hash_keys <- str_mask(mem, mask)
    for(j in 1:length(hash_keys)) {
      hash[[hash_keys[j]]] <- val
    }
  }
}

get_hash(ls(hash), hash) %>% sum() %>% formatC(digits = 13)
ls(hash) %>% sapply(function(x) bin2dec(x)) %>% max
