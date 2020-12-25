library(tidyverse)

dd <- read_lines("test")
dd <- data.frame(x = read_lines("test"))
pk <- 11562782
val <- 1
i <- 0

subject_number <- 7
while(val != pk) {
  val <- val * subject_number
  val <- val %% 20201227
  i <- i + 1
}  
card_key <- i


i <- 0
val <- 1
pk <- 18108497
while(val != pk) {
  val <- val * subject_number
  val <- val %% 20201227
  i <- i + 1
}  
door_key <- i

i <- 0
val <- 1
subject_number <- 18108497
while(i <= card_key - 1) {
  val <- val * subject_number
  val <- val %% 20201227
  i <- i + 1
}
val
