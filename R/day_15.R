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

repeat {
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
first_star #first star!
second_star #second_star!
