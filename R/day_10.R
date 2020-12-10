library(tidyverse)
library(igraph)
library(bit64)

dd <- as.integer(read_lines("data/day_10"))
dd <- sort(dd)
dd <- c(0, dd, max(dd) + 3)
(sum(diff(dd) == 3)) * (sum(diff(dd) == 1)) #first star!

locs <- paste(c(1, diff(dd)), collapse = "") %>% 
  str_locate_all("3") %>% 
  unlist() %>% 
  unique()

locs <- c(0, locs)
pp <- 1
for(x in 1:(length(locs) - 1)) {
  vv <- dd[locs[x]:(locs[x + 1])]
  mat <- outer(vv, vv, function(x, y) abs(x - y) > 0 & abs(x - y) <= 3 & x < y)
  gg <- graph_from_adjacency_matrix(mat)
  aa <- all_simple_paths(gg, from = 1, to = length(vv))
  v <- length(aa)
  pp <- pp * as.integer64(v)
}
pp #second star!
