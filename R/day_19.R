library(tidyverse)
library(igraph)


dd <- data.frame(x = read_lines("data/day_19_1")) %>% mutate(x = str_remove_all(x , "[\"]"))
dd <- separate(dd, col = x, into = c("V", "E"), sep = ":")
dd <- dd %>% separate_rows(E, sep = " \\| ") %>% 
  mutate(E = str_remove_all(E, "^ | $"))

aa <- dd %>% 
  group_by(V) %>% 
  mutate(count = n()) %>% 
  filter(count == 1)

vertices <- dd$V
edges <- dd$E
edges <- str_c(" ", edges, " ")

for(j in 1:nrow(aa)) {
  for(i in 1:nrow(aa)) {
  patt <- paste0(" ", aa$V[i], " ")
  replacement <- paste0(" ", aa$E[i], " ")
  edges <- str_replace_all(edges, patt, replacement)
  }
}

dd$E <- edges
dd <- dd %>% arrange(dd, V)

my_replace <- function(str, patt, replace) {
  ret_str <- character(0)
  for(i in 1:length(str)) {
    if(str_detect(str[i], patt)) {
      ret_str <- paste(ret_str, paste0(str_replace(str[i], patt, replace), collapse = ":"), sep = ":")
    } else {
      ret_str <- paste(ret_str, str[i], sep = ":")
    }
  }
  str_remove(ret_str, "^:")
}

edges <- dd$E[1]
edges #this is 42 42 31 for me

vertices <- dd$V

ll <- sapply(c(" 42 ", " 31 "), function(edges) {
  while(any(str_detect(edges, "[0-9]"))) {
    for(v in unique(dd$V)[-1]) {
      replacement <- dd %>% 
        filter(V == v) %>% 
        pull(E)
      patt <- paste0(" ", v, " ")
      if(any(str_detect(edges, patt))) {
        edges <- my_replace(edges, patt, replacement) %>% 
          str_split(pattern = ":") %>% 
          unlist()
      }
    }
  }
  edges
})

edges_42 <- ll[,1]
edges_31 <- ll[,2]

edges_42 <- edges_42 %>% str_remove_all(" ")
edges_31 <- edges_31 %>% str_remove_all(" ")

vals <- read_lines("data/day_19_2")
is_good <- function(x) {
  if(str_length(x) != 24) {
    return(FALSE)
  } else {
    str_sub(x, 1, 8) %in% edges_42 && str_sub(x, 9, 16) %in% edges_42 && str_sub(x, 17, 24) %in% edges_31
  }
}
sapply(vals, is_good) %>% 
  sum() #first star!


is_good_star_2 <- function(x) {
  N <- str_length(x)
  initial_42s <- 0
  while(str_sub(x, 1, 8) %in% edges_42) {
    initial_42s <- initial_42s + 1
    x <- str_sub(x, 9)
  }
  tail_31 <- 0
  while(str_sub(x, -8, -1) %in% edges_31) {
    tail_31 <- tail_31 + 1
    x <- str_sub(x, 1, -9)
  }
  return(tail_31 < initial_42s && tail_31 + initial_42s ==N/8 && initial_42s > 1 && tail_31 > 0)
}

sapply(vals, is_good_star_2) %>% sum() #second star!
