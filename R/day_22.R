library(collections)
library(tidyverse)

dd <- data.frame(x = read_lines("data/day_22"))
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
#' Hmm, this took about 3 hours on my machine, just as an FYI
#'

is_previous_game <- function(cg, ag) { #this is ridiculously slow!
  N <- max(ag$game)
  for(i in 1:N) {
    one_game <- ag %>% 
      filter(game == i) %>% 
      select(cards, player)
    if(identical(cg, one_game)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

play_game <- function(p1, p2) {
  all_games <- data.frame(cards = c(unlist(p1$as_list()), unlist(p2$as_list())),
                          player = c(rep(1, p1$size()), rep(2, p2$size())),
                          game = 1)
  while(p1$size() > 0 && p2$size() > 0) {
    c1 <- p1$peek()
    c2 <- p2$peek()
    if(c1 <= p1$size() - 1 && c2 <= p2$size() - 1) {

      q1 <- queue(items = p1$as_list()[2:(p1$peek() + 1)])
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
    
    current_game <- data.frame(cards = c(unlist(p1$as_list()), unlist(p2$as_list())),
                               player = c(rep(1, p1$size()), rep(2, p2$size())))
    
    N <- max(all_games$game) + 1
    if(N %% 1000 == 0) {
      if(is_previous_game(current_game, all_games)) {
        return(list(winner = 1, p1 = p1, p2 = p2))
      }
    }
    all_games <- bind_rows(all_games, 
                           data.frame(cards = c(unlist(p1$as_list()), unlist(p2$as_list())),
                            player = c(rep(1, p1$size()), rep(2, p2$size())),
                            game = N))
    if(N %% 200 == 0) {
      print(N)
    }
  }
  print(ppp)
  ppp <<- ppp + 1
  if(p1$size() == 0) { #player 2 wins
    return(list(winner = 2, p1, p2))
  } else{
    return(list(winner = 1, p1, p2))
  }
}

vall <- play_game(p1, p2)
if(vall$winner == 1) {
  ((vall[[2]]$as_list() %>% unlist()) * (vall[[2]]$size():1)) %>% sum()
} else {
  ((vall[[3]]$as_list() %>% unlist()) * (vall[[3]]$size():1)) %>% sum()
} #second star!

