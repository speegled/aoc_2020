library(tidyverse)

dd <- read_lines("data/day_24")
x <- dd[1]
parser <- function(x) {
  parsed <- character(0)
  y <- str_split(x, "") %>% 
    unlist()
  i <- 1
  while(i <= length(y)) {
    if(y[i] == "s" || y[i] == "n") {
      parsed <- c(parsed, paste0(y[i], y[i + 1], collapse = ""))
      i <-  i + 2
    } else {
      parsed <- c(parsed, y[i]) 
      i <- i + 1
    }
  }
  dirs <- data.frame(x = integer(0), y = integer(0))
  for(j in 1:length(parsed)) {
    if(parsed[j] == "e") {
      t <- data.frame(x = 1, y = 0)
    }
    if(parsed[j] == "w") {
      t <- data.frame(x = -1, y = 0)
    }
    if(parsed[j] == "se") {
      t <- data.frame(x = 1/2, y = -1)
    }
    if(parsed[j] == "ne") {
      t <- data.frame(x = 1/2, y = 1)
    }
    if(parsed[j] == "sw") {
      t <- data.frame(x = -1/2, y = -1)
    }
    if(parsed[j] == "nw") {
      t <- data.frame(x = -1/2, y = 1)
    }
    dirs <- bind_rows(dirs, t)  
  }
  dirs
}
parser(dd[1])
ff <- sapply(1:length(dd), function(i) {
  apply(parser(dd[i]), 2, function(x) round(sum(x), 3))
}) %>% 
  t() %>% 
  data.frame() %>% 
  group_by(x, y) %>% 
  summarize(count = n()) %>% 
  pull(count)

sum(ff %% 2 == 1) #first star!

flipped <- sapply(1:length(dd), function(i) {
  apply(parser(dd[i]), 2, function(x) sum(x))
}) %>% 
  t() %>% 
  data.frame() %>% 
  group_by(x, y) %>% 
  summarize(count = n(), .groups = "drop") %>%  
  filter(count %% 2 == 1)
flipped
a <- 1
add_vals <- c(1,0,-1,0,.5, a, .5, -a, -.5, a, -.5, -a, 0, 0)

ed <- function(x, y) {
  sqrt((vertices$V1[x] -vertices$V1[y])^2 + 3/4 * (vertices$V2[x] - vertices$V2[y])^2)
}
library(igraph)


vv <- flipped
for(i in 1:100) {

  vals <- matrix(c(vv$x[1], vv$y[1]) + add_vals, byrow = T, ncol = 2)
  for(j in 2:nrow(vv)) {
    vals <- rbind(vals, matrix(c(vv$x[j], vv$y[j]) + add_vals, byrow = T, ncol = 2))
  }
  #vv <- mutate_all(vv, ~round(., 8))
  vertices <- vals %>% 
    as.data.frame() %>% 
    distinct()
  
  edges <- outer(1:nrow(vertices), 1:nrow(vertices), FUN = function(x, y) ed(x, y)< 1.01)
  edges <- edges - diag(nrow(edges))
  gg <- graph_from_adjacency_matrix(edges)

  coords <- apply(vertices, 1, function(x) paste(x, collapse = " "))
  gg <- set_vertex_attr(gg, name = "coord", value = apply(vertices, 1, function(x) paste(x, collapse = " ")))
  
  gg <- set_vertex_attr(gg, name = "color", value = "white")
  for(j in 1:nrow(vv)) {
    indd <- which(paste(vv$x[j], vv$y[j], collapse = " ") == coords)
    gg <- set_vertex_attr(gg, name = "color", value = "black", index = V(gg)[indd])
  }
  switch_to_white <- integer(0)

  for(g in V(gg)[[color == "black"]]) {
    if(!sum(get.vertex.attribute(gg, name = "color", index = neighbors(gg, v = g)) == "black") %in% c(1, 2) ) {
      switch_to_white <- c(switch_to_white, g)
    }
    #print(sum(get.vertex.attribute(gg, name = "color", index = neighbors(gg, v = g)) == "black"))
    #print(g)
  }
  
  switch_to_black <- integer(0)
  for(g in V(gg)[[color == "white"]]) {
    if(sum(get.vertex.attribute(gg, name = "color", index = neighbors(gg, v = g)) == "black") == 2 ) {
      switch_to_black <- c(switch_to_black, g)
    }
  }
  
  if(i %% 10 == 1) {
    plot(gg, 
         mark.border = get.vertex.attribute(gg, name = "color"), 
         vertex.label = NA,
         vertex.size = 1,
         edge.arrow.size = 0,
         edge.arrow.width = 0)
  }
  
  gg <- set.vertex.attribute(gg, name = "color", index = switch_to_black, value = "black")
  gg <- set.vertex.attribute(gg, name = "color", index = switch_to_white, value = "white")
  
  vv <- data.frame(x = get.vertex.attribute(gg, V(gg)[[color == "black"]], name = "coord"))
  vv <- vv %>% separate(x, into = c("x", "y"), sep = " ", convert = T) 
  print(i)
  print(length(V(gg)[[color == "black"]]))
  print(" ")

}

length((V(gg)[[color == "black"]])) #second star!

