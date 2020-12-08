library(tidyverse)
library(igraph)


dd <- read_lines("data/day_07")

dd <- data.frame(x = dd)
head(dd)
edges <- dd %>% 
  extract(x, into = c("origin", "contains"), "([a-z ]+) bags (.*)") %>% 
  #mutate(origin = factor(origin)) %>% 
  mutate(contains = str_remove(contains, "contain[s]*")) %>%
  mutate(contains = str_remove_all(contains, ",")) %>% 
  mutate(contains = str_replace_all(contains, "([0-9]+)", "+\\1")) %>% 
  separate_rows(contains, sep = "\\+") %>% 
  filter(contains != " ") %>% 
  mutate(contains = str_remove(contains, "^ ")) %>% 
  mutate(contains = str_remove(contains, " bag[s]*[\\.]*")) %>% 
  mutate(contains = str_remove(contains, " $")) %>% 
  mutate(contains = str_replace(contains, "([0-9]+) ", "\\1+")) %>% 
  mutate(contains = ifelse(contains == "no other", "0+no other", contains)) %>% 
  # pull(contains) %>% 
  # str_detect("[0-9]+")
  separate(col = contains, into = c("weight", "contains"), sep = "\\+", convert = TRUE)

edges_no <- edges %>% 
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
