key <- "653427918"
library(tidyverse)
key <- str_split(key, "") %>% unlist() %>% as.integer()
N <- 9

#'
#' ll[i] is the value that follows the number i in the key.
#'

ll <- rep(0, N)
for(i in 1:(N - 1)) {
  ll[key[i]] <- key[i + 1]
}
ll[key[N]] <- key[1]

unwind <- function(x, start = 1, mmax = 100) {
  val <- start
  i <- x[start]
  j <- 1
  while(i != start && j < mmax) {
    val <- c(val, i)
    i <- x[i]
    j <- j + 1
  }
  val
}
unwind(ll, start = 6) #sanity check

start <- key[1]
for(i in 1:100) {
  head_of_three <- ll[start]
  mid_of_three <- ll[head_of_three]
  tail_of_three <- ll[mid_of_three]
  all_three <- c(head_of_three, mid_of_three, tail_of_three)
  parent_of_head_of_three <- start - 1
  if(parent_of_head_of_three == 0) {
    parent_of_head_of_three <- N
  }
  while(parent_of_head_of_three %in% all_three) {
    parent_of_head_of_three <- parent_of_head_of_three - 1
    if(parent_of_head_of_three == 0) {
      parent_of_head_of_three <- N
    }
  }
  
  child_of_tail_of_three <- ll[parent_of_head_of_three]
  ll[start] <- ll[tail_of_three]
  ll[parent_of_head_of_three] <- head_of_three
  ll[tail_of_three] <- child_of_tail_of_three
  start <- ll[start]
}

unwind(ll, start = 1)[-1] %>% paste(collapse = "") #first star!

key[10:1000000] <- 10:1000000
N <- 1000000
ll <- rep(0, N)
for(i in 1:(N - 1)) {
  ll[key[i]] <- key[i + 1]
}
ll[key[N]] <- key[1]
unwind(ll, start = 6) #sanity check
unwind(ll, start = 999998) #sanity check

start <- key[1]
for(i in 1:10000000) {
  head_of_three <- ll[start]
  mid_of_three <- ll[head_of_three]
  tail_of_three <- ll[mid_of_three]
  all_three <- c(head_of_three, mid_of_three, tail_of_three)
  parent_of_head_of_three <- start - 1
  if(parent_of_head_of_three == 0) {
    parent_of_head_of_three <- N
  }
  while(parent_of_head_of_three %in% all_three) {
    parent_of_head_of_three <- parent_of_head_of_three - 1
    if(parent_of_head_of_three == 0) {
      parent_of_head_of_three <- N
    }
  }
  
  child_of_tail_of_three <- ll[parent_of_head_of_three]
  ll[start] <- ll[tail_of_three]
  ll[parent_of_head_of_three] <- head_of_three
  ll[tail_of_three] <- child_of_tail_of_three
  start <- ll[start]
}

unwind(ll, start = 1, mmax = 3) %>% prod() #second star!