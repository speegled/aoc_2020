#'
#' Cleaned up a little. Stil don't like it.
#' 

library(tidyverse)

dd <- data.frame(x = read_file("data/day_16"))
tickets <- dd %>% 
  transmute(value = str_extract(x, "(?<=nearby tickets:\\n)[\\s\\S]*?(?=\\n$)")) %>% 
  separate_rows(value, sep = "\\n", convert = TRUE) %>% 
  mutate(ticket = 1:n()) %>% 
  separate_rows(value, sep = ",", convert = TRUE) %>% 
  group_by(ticket) %>% 
  mutate(position = 1:n())

rules <- dd %>% 
  transmute(rule = str_extract(x, "[\\s\\S]*?(?=\\n\\nyour)")) %>% 
  separate_rows(rule, sep = "\\n") %>% 
  separate(rule, into = c("description", "value"), sep = ": ") %>% 
  separate_rows(value, sep = " or ") %>% 
  separate(value, into = c("lower", "upper"), sep = "-", convert = TRUE)

ff <- full_join(tickets, rules, by = character())

ff %>% 
  group_by(ticket, position)  %>% 
  summarize(is_valid = any(value <= upper & value >= lower), value = value) %>% 
  filter(!is_valid) %>% 
  distinct() %>% 
  pull(value) %>% 
  sum() #first star!

part_2 <- ff %>% 
  group_by(ticket, position)  %>% 
  mutate(is_valid = any(value <= upper & value >= lower)) %>% 
  group_by(ticket) %>% 
  filter(all(is_valid))

part_2 <- part_2 %>% 
  group_by(position, description, ticket) %>% 
  summarize(is_good = any(value >= lower & value <= upper)) %>% 
  group_by(position, description) %>% 
  summarize(is_good = all(is_good)) %>% 
  group_by(position) %>% 
  mutate(num_good = sum(is_good)) %>% 
  arrange(num_good) %>% 
  filter(is_good)

description <- character(0)
position <- integer(0)
for(d in unique(part_2$position)) {
  position <- c(position, setdiff(filter(part_2, position == d) %>% 
                                    pull(position), position))
  description <- c(description, setdiff(filter(part_2, position == d) %>% 
                                          pull(description), description))
}

my_ticket <- c(103,79,61,97,109,67,89,83,59,53,139,131,101,113,149,127,71,73,107,137)
prod(my_ticket[position[which(str_detect(description, "^departure"))]]) %>% formatC(digits = 13) #second star!



#'
#' Live work
#'

library(tidyverse)

dd <- read_file("data/day_16")
dd <- dd %>% 
  str_split_fixed("\n\n", n = 2)
aa <- dd[,1]
bb <- dd[,2]

field_names <- aa %>% 
  str_split(":") %>% 
  unlist() %>% 
  str_extract("[a-z ]*$")

field_names <- field_names[-length(field_names)]

aa <- aa %>% 
  str_remove_all("[a-z:\n]") %>% 
  #str_remove_all("[\n]") %>% 
  str_replace_all("  ", " ") %>% 
  str_replace("^ ", "") %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  extract(x, into = c("lower", "upper"), "([0-9]+)+-([0-9]+)", convert = T) 

aa <- aa %>% 
  mutate(field_name = rep(field_names, each = nrow(aa)/length(field_names)))

bb <- bb %>% 
  str_remove_all("[a-z: ]") %>%  
  str_remove_all("^\n|\\n$") %>% 
  str_split(pattern = "\\n+") %>% 
  unlist() %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>% 
  mutate(ticket = 1:nrow(.)) %>%  
  separate_rows(x, sep = ",", convert = T)

full_join(bb, aa, by = character()) %>% 
  group_by(x, ticket) %>% 
  summarize(is_good = any(x >= lower & x <= upper)) %>%
  ungroup() %>% 
  filter(!is_good) %>% 
  summarize(sum(x)) #first star!

nn <- sum(bb$ticket == 1)
bb <- bb %>% 
  mutate(position = rep(1:nn, times = nrow(bb)/nn))

cc <- full_join(bb, aa, by = character()) %>% 
  group_by(x, ticket) %>% 
  mutate(is_good = any(x >= lower & x <= upper)) %>%
  ungroup() %>% 
  filter(is_good)

dd <- cc %>% 
  filter(ticket!= 1)
dd
#dd$field_name <-  rep(field_names, nrow(dd)/length(field_names))
dd
order <- dd %>% 
  mutate(is_good = x >= lower & x <= upper) %>% 
  #filter(field_name == "class") %>% 
  group_by(field_name, position, ticket) %>% 
  summarize(valid_field = any(is_good)) %>% 
  summarize(valid_position = all(valid_field)) %>% 
  ungroup() %>% 
  filter(valid_position) %>% 
  count(field_name) %>% 
  arrange(n)
order$field_name
field <- order$field_name[2]
position <- numeric(0)
ee <- dd
for(field in order$field_name) {
  positions <- ee %>% 
    mutate(is_good = x >= lower & x <= upper) %>% 
    filter(field_name == field) %>% 
    group_by(field_name, position, ticket) %>% 
    summarize(valid_field = any(is_good)) %>% 
    summarize(valid_position = all(valid_field)) %>% 
    ungroup() %>% 
    filter(valid_position) %>% 
    pull(position)
  position <- c(position, setdiff(positions, position))
}

prod(bb$x[position[which(str_detect(order$field_name, "^departure"))]]) %>% formatC(digits = 13)
position
position[which(str_detect(order$field_name, "^departure"))]
