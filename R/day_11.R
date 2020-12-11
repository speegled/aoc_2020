library(tidyverse)
library(igraph)

dd <- read_lines("data/day_11")
N <- length(dd)
mat <- str_split(dd, pattern = "") %>% 
  unlist() %>% 
  matrix(nrow = length(dd), byrow = T) 

chairs <- which(mat == "L", arr.ind = T)
chairs <- chairs %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(index = as.integer(rowname))

is_edge <- function(x, y) {
  rd <- abs(chairs$row[x] - chairs$row[y])
  cd <- abs(chairs$col[x] - chairs$col[y])
  (rd == 1 & cd == 0) | (rd == 0 & cd == 1) | rd * cd == 1
}

edges <- outer(chairs$index, chairs$index, function(x, y) is_edge(x, y))
gg <- graph_from_adjacency_matrix(edges)
gg <- set_vertex_attr(gg, name = "occupied", value = "false")

neighbors_gg <- sapply(1:length(V(gg)), function(x) {
  neighbors(gg, v = x)
})

change_to_occupied <- function(gg) {
  sapply(1:length(V(gg)), function(x) {
    nn <- neighbors_gg[[x]]
    if(all(vertex_attr(gg, name = "occupied", index = nn) == "false") && V(gg)[x][[]]$occupied == "false") {
      x
    } else{ 
      NA}
  })
}

change_to_unoccupied <- function(gg) {
  sapply(1:length(V(gg)), function(x) {
    nn <- neighbors_gg[[x]]
    if(sum(vertex_attr(gg, name = "occupied", index = nn) == "true") >= 4 && V(gg)[x][[]]$occupied == "true") {
      x
    }
    else {
      NA
    }
  })
}
i <- 1
repeat {
  unocc <- change_to_unoccupied(gg)
  occ <- change_to_occupied(gg)
  unocc <- unocc[!is.na(unocc)]
  occ <- occ[!is.na(occ)]
  
  if(length(occ) + length(unocc) == 0) {
    break
  }
  
  gg <- set_vertex_attr(gg, name = "occupied", index = occ, value = "true")
  gg <- set_vertex_attr(gg, name = "occupied", index = unocc, value = "false")
  print(i)
  i <- i + 1
}

sum(vertex_attr(gg, name = "occupied", index = V(gg)) == "true") #first star!



#'
#' Second star: just need to change the adjacency matrix.
#'



max_col <- ncol(mat)
max_row <- nrow(mat)

dirs <- list(c(1, 0), c(0, 1), c(-1, 0), c(0, -1), c(1, 1), c(-1, 1), c(1, -1), c(-1, -1))
get_neighbors <- function(x, max_col, max_row) {
  pos <- c(chairs$row[x], chairs$col[x])
  rc <- paste(chairs$row, chairs$col)
  sapply(dirs, function(y) {
    i <- 1
    cpos <- pos + i * y
    while(all(cpos <= c(max_row, max_col)) && all(cpos > 0)) {
      if(paste(cpos, collapse = " ") %in% rc) {
        return(which(paste(cpos, collapse = " ") == rc))
      }
      i <- i + 1
      cpos <- pos + i * y
    }
    return(NA)
  })
}
neighbors_gg <- lapply(1:nrow(chairs), function(a) {
  nn <- get_neighbors(a, max_col = max_col, max_row = max_row)
  nn[!is.na(nn)]})

mat <- matrix(FALSE, nrow = nrow(chairs), ncol = nrow(chairs))

for(i in 1:length(neighbors_gg)) {
  mat[i, neighbors_gg[[i]]] <- TRUE
}

isSymmetric(mat) #sanity check

gg <- graph_from_adjacency_matrix(mat)
gg <- set_vertex_attr(gg, name = "occupied", value = "false")

neighbors_gg <- sapply(1:length(V(gg)), function(x) {
  neighbors(gg, v = x)
}) #this is annoying


change_to_occupied <- function(gg) {
  sapply(1:length(V(gg)), function(x) {
    nn <- neighbors_gg[[x]]
    if(all(vertex_attr(gg, name = "occupied", index = nn) == "false") && V(gg)[x][[]]$occupied == "false") {
      x
    } else{ 
      NA}
  })
}

change_to_unoccupied <- function(gg) {
  sapply(1:length(V(gg)), function(x) {
    nn <- neighbors_gg[[x]]
    if(sum(vertex_attr(gg, name = "occupied", index = nn) == "true") >= 5 && V(gg)[x][[]]$occupied == "true") { #changed to 5
      x
    }
    else {
      NA
    }
  })
}
i <- 1
repeat {
  unocc <- change_to_unoccupied(gg)
  occ <- change_to_occupied(gg)
  unocc <- unocc[!is.na(unocc)]
  occ <- occ[!is.na(occ)]
  
  if(length(occ) + length(unocc) == 0) {
    break
  }
  
  gg <- set_vertex_attr(gg, name = "occupied", index = occ, value = "true")
  gg <- set_vertex_attr(gg, name = "occupied", index = unocc, value = "false")
  print(i)
  i <- i + 1
}

sum(vertex_attr(gg, name = "occupied", index = V(gg)) == "true") #second star!





