library(collections)
library(tidyverse)

dd <- data.frame(x = read_lines("../aoc_2020/data/day_22"))
dd <- dd %>% mutate(player = cumsum(x == "") + 1) %>% 
  filter(!str_detect(x, "Player")) %>% 
  filter(x != "")

cards_1 <- dd %>% filter(player == 1) %>% pull(x)
p1 <- queue(items = as.integer(cards_1))

cards_2 <- dd %>% filter(player == 2) %>% pull(x)
p2 <- queue(items = as.integer(cards_2))

while(!p1$size() == 0 && !p2$size() == 0) {
  p1_curr <- p1$pop()
  p2_curr <- p2$pop()
  if(p1_curr > p2_curr) {
    p1 <- p1$push(p1_curr)
    p1 <- p1$push(p2_curr)
  } else {
    p2 <- p2$push(p2_curr)
    p2 <- p2$push(p1_curr)
  }
}

if(p2_curr > p1_curr) {
  vals <- p2$as_list() %>% 
    unlist()
} else {
  vals <- p1$as_list() %>% 
    unlist()
}
(vals * (length(vals):1)) %>% sum() #first star!

#'
#' this next part takes about 5 minutes
#'

is_previous_game <- function(cg, ag) {
  return(cg %in% ag)
}

play_game <- function(p1, p2) {
  all_games <- digest::digest(list(p1, p2)) 
  while(p1$size() > 0 && p2$size() > 0) {
    N <- 1
    c1 <- p1$peek()
    c2 <- p2$peek()
    if(c1 <= p1$size() - 1 && c2 <= p2$size() - 1) {
      
      q1 <- queue(items = p1$as_list()[2:(p1$peek() + 1)]) #this is the slowdown, I think. wishing I hadn't used a queue
      q2 <- queue(items = p2$as_list()[2:(p2$peek() + 1)])
      
      recursive_result <- play_game(q1, q2)
      if(recursive_result$winner == 1) {
        p1 <- p1$push(p1$pop())
        p1 <- p1$push(p2$pop())
      } else {
        p2 <- p2$push(p2$pop())
        p2 <- p2$push(p1$pop())
      }
      #end of recursive combat 
    } else {
      if(c1 > c2) {
        p1 <- p1$push(p1$pop())
        p1 <- p1$push(p2$pop())
      } else {
        p2 <- p2$push(p2$pop())
        p2 <- p2$push(p1$pop())
      }
    } #end of one round
    
    current_game <- digest::digest(list(p1, p2))
    
    N <- length(all_games)
    if(N %% 1000 == 0) {
      print(N)
      if(is_previous_game(current_game, all_games)) {
        return(list(winner = 1, p1 = p1, p2 = p2))
      }
    }
    all_games <- c(all_games, current_game)
  }

  if(p1$size() == 0) { #player 2 wins
    return(list(winner = 2, p1, p2))
  } else{
    return(list(winner = 1, p1, p2))
  }
}

cards_1 <- dd %>% filter(player == 1) %>% pull(x)
p1 <- queue(items = as.integer(cards_1))

cards_2 <- dd %>% filter(player == 2) %>% pull(x)
p2 <- queue(items = as.integer(cards_2))

vall <- play_game(p1, p2)
if(vall$winner == 1) {
  ((vall[[2]]$as_list() %>% unlist()) * (vall[[2]]$size():1)) %>% sum()
} else {
  ((vall[[3]]$as_list() %>% unlist()) * (vall[[3]]$size():1)) %>% sum()
} #second star!

