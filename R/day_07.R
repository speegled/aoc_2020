library(tidyverse)
library(igraph)

dd <- data.frame(x = read_lines("data/day_07"))

edges_no <- dd %>% 
  mutate(origin = str_extract(x, "^[a-z ]*(?= bag)")) %>% 
  mutate(contains = str_extract_all(x, "[0-9].*")) %>% 
  mutate(contains = str_remove_all(contains, " bag[s]*|\\.|")) %>% 
  filter(str_length(contains) > 0) %>% 
  separate_rows(contains, sep = ", ") %>% 
  extract(contains, into = c("weight", "contains"), regex = "([0-9]) (.*)", convert = TRUE) %>% 
  select(-x) %>% 
  filter(contains != "no other")

gg <- graph_from_edgelist(el = as.matrix(edges_no[,c(1,3)]), directed = TRUE)

aa <- distances(gg, v = V(gg), to = "shiny gold", mode = "out")
sum(aa < Inf & aa > 0) #first star!
 

gg <- set_edge_attr(gg, name = "weight", value = edges_no$weight)
nbs <- neighborhood(gg, nodes = "shiny gold", mode = "out", order = 1000)
all_paths <- all_simple_paths(gg,from = "shiny gold", to = nbs[[1]], mode = "out")
sapply(1:length(all_paths), function(x) {
  prod(E(gg, path = all_paths[[x]])$weight)
}) %>% 
  sum() #second star!





#'
#' original way below
#'

gg <- set_edge_attr(gg, name = "weight", value = edges_no$weight)
nbs <- neighborhood(gg, nodes = "shiny gold", mode = "out", order = 1000)
all_paths <- all_simple_paths(gg,from = "shiny gold", to = nbs[[1]], mode = "out")
boxes_in_path <- function(xx, gg = gg) {
  sapply(1:(length(xx) - 1), function(j) {
    distances(gg, xx[j], xx[j + 1], mode = "out")
  }) %>% prod()
}
sapply(all_paths, function(xx) {
  boxes_in_path(xx, gg)
}) %>% sum() #second star!
