library(tidyverse)

pk <- 11562782 #my data
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
pk <- 18108497 #more my data
while(val != pk) {
  val <- val * subject_number
  val <- val %% 20201227
  i <- i + 1
}  
door_key <- i

i <- 0
val <- 1
subject_number <- 18108497 #this is pk from above :-)
while(i <= card_key - 1) {
  val <- val * subject_number
  val <- val %% 20201227
  i <- i + 1
}
val
