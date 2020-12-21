library(tidyverse)

dd <- read_lines("data/day_21")
allergens <- dd %>% str_extract_all(pattern = "(?<=\\(contains ).*?(?=\\))") %>% unlist()
ingredients <- dd %>% str_remove_all(pattern = " \\(.*\\)")

df <- data.frame(allergens, ingredients)
df$recipe <- 1:nrow(df)
df <- df %>% 
  separate_rows(allergens, sep = ", ") %>% 
  separate_rows(ingredients, sep = " ")

possible_alergens <- df %>% 
  group_by(allergens) %>% 
  mutate(allergen_count = length(unique(recipe))) %>% 
  group_by(allergens, ingredients) %>% 
  mutate(ingredient_count = n()) %>% 
  group_by(allergens) %>%
  filter(allergen_count == ingredient_count) %>% 
  distinct(allergens, ingredients, allergen_count, ingredient_count)

dd <- possible_alergens %>% 
  filter(ingredient_count == -1)

repeat{
  new_knowledge <- possible_alergens %>% 
    group_by(allergens) %>% 
    filter(n() == 1)
  dd <- bind_rows(dd, new_knowledge)
  if(nrow(new_knowledge) == 0) {
    break
  }
  possible_alergens <- possible_alergens %>% 
    filter(!(ingredients %in% new_knowledge$ingredients))
}

df %>% distinct(ingredients, recipe) %>% 
  filter(!(ingredients %in% dd$ingredients)) %>% 
  nrow() #first star!

dd %>% 
  arrange(allergens) %>% 
  pull(ingredients) %>% 
  paste(collapse= ",") #second star!
