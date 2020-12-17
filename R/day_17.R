#'
#' This is the code for the second star. Code for first star in comments
#'


library(tidyverse)
library(igraph)

all_sums <- combinat::hcube(rep(3, 4), translation = -2)
all_sums <- all_sums[-41,]
# all_sums <- combinat::hcube(rep(3, 3), translation = -2)
# all_sums <- all_sums[-14,]


get_neighbors <- function(start, max_graph = 20) {
  xint <- as.integer(strsplit(start, ",", fixed = TRUE)[[1]])
  potential <- xint + t(all_sums)
  potential[,apply(potential, 2, function(x) all(x > 0) & all(x <= max_graph))] %>% 
    apply(2, function(x) paste(x, collapse = ","))
}

max_graph <- 20
vertices <- combinat::hcube(rep(max_graph, 4)) %>% 
  apply(1, function(x) paste(x, collapse = ","))
# vertices <- combinat::hcube(rep(max_graph, 3)) %>% 
#   apply(1, function(x) paste(x, collapse = ","))

graph_data_frame <- bind_rows(lapply(vertices, function(start) {
  data.frame(v1 = start, v2 = get_neighbors(start))
}))

gg <- graph_from_data_frame(d = graph_data_frame)
gg_save <- gg

initial_vertices <- apply(combinat::hcube(x = c(8,8,1,1), translation = c(6,6,9,9)), 1, function(x) paste(x, collapse = ","))
# initial_vertices <- apply(combinat::hcube(x = c(8,8,1), translation = c(6,6,9)), 1, function(x) paste(x, collapse = ","))

reachable_vertices <- neighborhood(gg, order = 6, nodes = initial_vertices) %>% unlist() %>% unique()
gg <- subgraph(gg, reachable_vertices)

dd <- read_file("data/day_17") %>% 
  str_remove_all("\\n") %>% 
  str_split("") %>% 
  unlist() %>% 
  matrix(nrow = 8, byrow = T)

gg <- set_vertex_attr(gg, name = "power", value = "off")
power_on <- which(dd == "#", arr.ind = T) + 
  c(6, 6)
power_on <- apply(power_on, 1, function(x) paste(c(x, "10", "10"), collapse = ","))
# power_on <- apply(power_on, 1, function(x) paste(c(x, "10"), collapse = ","))
gg <- set_vertex_attr(gg, name = "power", index = V(gg)[power_on], value = "on")

change_to_off <- function(gg) {
  sapply(V(gg)[[power == "on"]], function(x) {
    nn <- neighbors(gg, v = x)
    if(sum(vertex_attr(gg, name = "power", index = nn) == "on") %in% c(2, 3)) {
      FALSE
    } else{ 
      TRUE
    }
  })
}

change_to_on <- function(gg, i) {
  verts <- neighborhood(gg, order = 1, nodes = V(gg)[[power == "on"]]) %>% unlist() %>% unique()
  sapply(verts, function(x) {
    nn <- neighbors(gg, v = x)
    if(sum(vertex_attr(gg, name = "power", index = nn) == "on") == 3 && get.vertex.attribute(gg, name = "power", index = x) == "off") {
      TRUE
    } else{ 
      FALSE
    }
  })
}

system.time(for(i in 1:6) {
  turn_off <- change_to_off(gg)
  turn_on <- change_to_on(gg, i)
  current_on <-  V(gg)[[power == "on"]]
  verts_off <- neighborhood(gg, order = 1, nodes = V(gg)[[power == "on"]]) %>% unlist() %>% unique()
  gg <- set.vertex.attribute(gg, name = "power", index = current_on[turn_off], value = "off")
  gg <- set.vertex.attribute(gg, name = "power", index = verts_off[turn_on], value = "on")
  print(i)
  print(length(V(gg)[[power == "on"]]))
})
length(V(gg)[[power == "on"]]) #second star!
