#'
#' Done two ways. First way is better and uses collections package.
#'

library(collections)

key <- as.character(c(2,0,1,9,5,19))
value <- 1:length(key)

hash <- dict(items = value, keys = key)

current_iteration <- length(key)
current_value <- 19

for(current_iteration in length(key):2019) {
  hash_key <- as.character(current_value)
  current_value <- current_iteration - hash$get(hash_key, default = current_iteration)
  hash$set(hash_key, current_iteration) 
}
current_value #first star!

system.time(expr = for(current_iteration in 2020:(30000000 - 1)) {
  hash_key <- as.character(current_value)
  current_value <- current_iteration - hash$get(hash_key, default = current_iteration)
  hash$set(hash_key, current_iteration) 
}) #130 seconds on my computer
current_value #second star!


#'
#' Below was how I did it "live." Found out later that doing hashes this way
#' has a memory leak and possibly related this code runs a lot slower, even
#' if I clean up the if() statement and swith to a for loop.
#'

hash <- new.env(hash = TRUE, parent = emptyenv(), size = 30000001)
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))

key <- as.character(c(2,0,1,9,5,19))
value <- 1:length(key)
assign_hash(key, value, hash)

current_iteration <- length(key)
hash_key <- key[length(key)]
if(exists(hash_key, hash)) {
  current_value<- current_iteration - hash[[hash_key]]
} else {
  current_value <- 0
}
current_iteration <- current_iteration + 1

system.time(expr = repeat {
  hash_key <- as.character(current_value)
  if(exists(hash_key, hash)) {
     current_value<- current_iteration - hash[[hash_key]]
  } else {
    current_value <- 0
  }
  hash[[hash_key]] <- current_iteration

  current_iteration <- current_iteration + 1
  if(current_iteration %in% c(2020, 30000000)) {
    if(current_iteration == 2020) {
      first_star <- current_value
    } else {
      second_star <- current_value
      break
    }
  }
} 
)
first_star #first star!
second_star #second_star!
