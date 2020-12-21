library(ttidyverse)
library(collections)

dd <- data.frame(x = read_lines("test"))
dd <- dd %>% 
  mutate(image = cumsum(x == "")) %>% 
  filter(x != "") 

names <- dd %>% 
  group_by(image) %>% 
  summarize(name = str_extract(first(x), "[0-9]+"))

dd <- filter(dd, !str_detect(x, "[0-9]"))
dd <- left_join(dd, names)
dd
mats <- lapply(unique(dd$name), function(x) {
  dd[dd$name == x,]
})

mm <- list(0)
for(i in 1:length(mats)) {
  mm[[i]] <- str_split(paste0(mats[[i]]$x), "") %>% unlist() %>% 
    matrix(ncol = 10, byrow = T)
}

da <- dict(items = mm, keys = names$name)
da$as_list()

names <- names$name

match_rows <- sapply(names, function(x) {
  c(paste0(da$get(x)[1,], collapse = ""), 
    paste0(da$get(x)[10,], collapse = ""), 
    paste0(da$get(x)[,10], collapse = ""), 
    paste0(da$get(x)[,1], collapse = ""), 
    paste0(rev(da$get(x)[1,]), collapse = ""), 
    paste0(rev(da$get(x)[10,]), collapse = ""), 
    paste0(rev(da$get(x)[,10]), collapse = ""), 
    paste0(rev(da$get(x)[,1]), collapse = "")
  )
})

bounds <- dict(items = list(match_rows[1:8,1]), keys = names[1])
for(i in names[-1]) {
  bounds$set(key = i, value = match_rows[1:8, i])
}
N <- ncol(match_rows)
positions <- matrix(rep(0, N^2), nrow = N)
rownames(positions) <- names
colnames(positions) <- names
for(i in names) {
  for(j in names) {
    positions[i, j] <- any(sapply(bounds$get(key = i), function(x) x %in% bounds$get(key = j)))
  }
}
positions <- positions - diag(N)
attributes(which(apply(positions, 2, sum) == 2))$names %>% as.integer() %>% prod() %>% format(scientific = F) #first star!

source("R/day_20/helpers.R")
rotate <- function(x) t(apply(x, 2, rev))
corners <- attributes(which(apply(positions, 2, sum) == 2))$names 
edges <-  attributes(which(apply(positions, 2, sum) == 3))$names 
length(edges)

gg <- graph_from_adjacency_matrix(positions)
gg_full <- gg
gg <- subgraph(gg, v = c(corners, edges))
corners

#edge_paths <- igraph::all_shortest_paths(gg, from = "2953", to = corners) #this tells me how to set up all the edges
edge_paths <- igraph::all_shortest_paths(gg, from = "1753", to = corners) #this tells me how to set up all the edges
bb <- matrix(rep(0, 14400), nrow = 10 * 12)
edge_paths
#'
#' Find orientation of first corner matrix; number 1753. We know it matches to 2693 and 1609
#'

edge_paths[[1]]

sapply(bounds$get(key = "1753"), function(x) x %in% bounds$get(key = "2693")) #[,10] not reversed
sapply(bounds$get(key = "1753"), function(x) x %in% bounds$get(key = "1609")) #[10,] reversed 

#'
#' if we transpose and then rotate by 90 degrees clockwise and put in top right corner, then that should work.
#'

row <- 1
col <- 12

V(gg)
input_mat <- rotate(t(da$get(key = "1753")))
input_mat
edge <- paste(input_mat[,1], collapse = "")
edge
bb <- put_matrix(input_mat, bb, row, col)
bb
next_mat <- "1753"
j <- 1
for(j in 1:11) {
  col <- 12 - j
  curr_mat <- next_mat
  next_mat <- vertex_attr(gg, name = "name", edge_paths$res[[2]][[j + 1]])
  mat <- da$get(key = next_mat)
  input_mat <- find_edge_orientation(mat, edge, "right")
  input_mat
  edge <- paste(input_mat[,1], collapse = "")
  bb <- put_matrix(input_mat, bb, row, col)
  bb  
}

edge <- paste(input_mat[10,], collapse = "")

for(j in 1:11) {
  row <- 1 + j
  curr_mat <- next_mat
  next_mat <- vertex_attr(gg, name = "name", edge_paths$res[[3]][12 + j])
  mat <- da$get(key = next_mat)
  input_mat <- find_edge_orientation(mat, edge, "top")
  input_mat
  edge <- paste(input_mat[10,], collapse = "")
  bb <- put_matrix(input_mat, bb, row, col)
  bb  
}

edge <- paste(input_mat[,10], collapse = "")
edge_paths <- all_shortest_paths(gg, from = "3083", to = "1489")

for(j in 1:11) {
  col <- 1 + j
  curr_mat <- next_mat
  next_mat <- vertex_attr(gg, name = "name", edge_paths$res[[1]][1 + j])
  mat <- da$get(key = next_mat)
  input_mat <- find_edge_orientation(mat, edge, "left")
  input_mat
  edge <- paste(input_mat[,10], collapse = "")
  bb <- put_matrix(input_mat, bb, row, col)
  bb  
}

edge <- paste(input_mat[1,], collapse = "")
edge_paths <- all_shortest_paths(gg, from = "1489", to = "1753")

for(j in 1:10) {
  row <- 12 - j
  curr_mat <- next_mat
  next_mat <- vertex_attr(gg, name = "name", edge_paths$res[[1]][1 + j])
  mat <- da$get(key = next_mat)
  input_mat <- find_edge_orientation(mat, edge, "bottom")
  input_mat
  edge <- paste(input_mat[1,], collapse = "")
  bb <- put_matrix(input_mat, bb, row, col)
  bb  
}

left_edge <- all_shortest_paths(gg_full, from = "2843", to = "3083")
right_edge <- all_shortest_paths(gg_full, from = "1753", to = "1489")
i <- 2
j <- 3
for(i in 2:11) {
  left_mat <- left_edge$res[[1]][i]
  right_mat <- right_edge$res[[1]][i]
  edge_paths <- all_shortest_paths(gg_full, from = left_mat, to = right_mat)
  for(j in 2:11) {
    cur_mat <- vertex_attr(gg_full, name = "name", index = edge_paths$res[[1]][j - 1])
    next_mat <- vertex_attr(gg_full, name = "name", index = edge_paths$res[[1]][j])
    rows <- ((i - 1) * 10 + 1):(i * 10)
    edge <- paste(bb[rows, (j - 1) * 10], collapse = "")
    mat <- da$get(key = next_mat)
    input_mat <- find_edge_orientation(mat, edge, "left")
    bb <- put_matrix(input_mat, bb, row = i, col = j)
  }
}

#'
#' sanity check
#'

sapply(1:11, function(x) all(bb[,x * 10] == bb[,x * 10 + 1]))
sapply(1:11, function(x) all(bb[x*10,] == bb[x*10 + 1,]))

remove_vals <- c(1:11 * 10, 1:11 * 10 + 1)
# bb_save <- bb
bb <- bb[,-remove_vals]
bb <- bb[-remove_vals,]

write_delim(as.data.frame(bb), path = "bbsmall")

ss <- read_lines("seamonster")
#ss <- ss %>% str_replace_all("\\#", "1") %>% 
#  str_replace_all(" ", "0")
ss <- paste(ss, collapse = "")
ss
ss <- ss %>% str_split("") %>% unlist() %>%  
  matrix(byrow = T, nrow = 3)
ss
ssind <- which(ss == "#", arr.ind = T)
ssind
ssind %>% str()
dim(bb)
monster <- matrix(rep(0, 98^2), ncol = 98)

monster[which(bb == "#", arr.ind = T)] <- "#"
sum(monster == "#")
sum(bb == "#")
i <- 1
j <- 2


for(k in 1:4) {
  ss <- rotate(ss)
  ssind <- which(ss == "#", arr.ind = T)
  for(i in 0:(98 - nrow(ss))) {
    for(j in 0:(98 - ncol(ss))) {
      mat_add <- matrix(c(rep(i, 15), rep(j, 15)), ncol = 2)
      sstemp <- ssind + mat_add
      if(all(bb[sstemp] == "#")) {
        monster[sstemp] <- "."
        print(k)
      }
    }
  }
}
sum(monster == "#")
ss <- t(ss)
for(k in 1:4) {
  ss <- rotate(ss)
  ssind <- which(ss == "#", arr.ind = T)
  for(i in 0:(98 - nrow(ss))) {
    for(j in 0:(98 - ncol(ss))) {
      mat_add <- matrix(c(rep(i, 15), rep(j, 15)), ncol = 2)
      sstemp <- ssind + mat_add
      if(all(bb[sstemp] == "#")) {
        num_monst <- num_monst + 1
        print(k)
        monster[sstemp] <- "."
      }
    }
  }
}

which(monster == "#")
sum(monster == "#")
write_delim(data.frame(monster), path = "monster")






ss
ssind
all(monster[which(bb == "#")]== "#")
ssind
ssind %>% as.vector()
bb[1:20, 1:3]
bb[ssind]
ssind
sapply(1:15, function(x) bb[ssind[x,1], ssind[x,2]])
bb[1:3, 1:20]
bb[ssind]
ssind
for(i in 0:78) {
  for(j in 0:95) {
    mat_add <- matrix(c(rep(i, 15), rep(j, 15)), ncol = 2)
    sstemp <- ssind + mat_add
    if(all(bb[sstemp] == "#")) {
      monster[sstemp] <- "."
    }
  }
}
sum(monster == "#")

vvv <- monster %>% str_replace_all("#", "1") %>% 
  str_replace_all("\\.", "2") %>% 
  str_replace_all("0", "0") %>% 
  as.integer() %>% 
  matrix(ncol = 98) 


image(vvv/3)
monster

