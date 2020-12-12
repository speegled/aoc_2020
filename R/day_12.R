library(tidyverse)

#'
#' helper function to deal with indexing starting with 1. still confuses me otherwise.
#'

inc <- function(x, n, m = 4) {
  a <- (x + n) %% 4
  if(a == 0) {
    a <- m
  }
  a
}

dd <- data.frame(x = read_lines("data/day_12"))
dd <- dd %>% 
  extract(col = x, into = c("direction", "distance"), regex = "([A-Z])([0-9]+)", convert = T)


dirs <- list(c(1,0), c(0,1), c(-1,0), c(0,-1))

move <- function(direction, distance, current) {
  current <- case_when(
    direction == "N" ~ current + distance * c(0, 1),
    direction == "S" ~ current + distance * c(0, -1),
    direction == "E" ~ current + distance * c(1, 0),
    direction == "W" ~ current + distance * c(-1, 0),
    direction == "L" ~ current,
    direction == "R" ~ current,
    direction == "F" ~ current + distance * dirs[[facing]]
  )
  if(direction == "R") {
    facing <<- inc(facing, -round(distance/90)) #updates facing in the parent environment
  }
  if(direction == "L") {
    facing <<- inc(facing, round(distance/90))
  }
  return(current)
}

facing <- 1
current <- c(0, 0)
i <- 1
for(i in 1:nrow(dd)) {
  current <- move(dd$direction[i], dd$distance[i], current)
  print(c(current, facing))
}
sum(abs(current))  #first star!

#'
#' Start of second star
#'

move_left <- function(wp, di) {
  #assume di a multiple of 90
  di <- (di / 90) 
  for(j in 1:di) {
    wp <- rev(wp)
    wp[1] <- -wp[1]
  }
  return(wp)
  # added after the fact, but seems worse
  mat <- matrix(c(0, 1, -1, 0), nrow = 2)
  return(as.vector(matrixcalc::matrix.power(x = mat, k = di) %*% wp))
}

move_right <- function(wp, di) {
  di <- (di / 90) 
  for(j in 1:di) {
    wp <- rev(wp)
    wp[2] <- -wp[2]
  }
  return(wp)
}

move_waypoint <- function(direction, distance, waypoint, ship) {
  current <- case_when(
    direction == "N" ~ waypoint + distance * c(0, 1),
    direction == "S" ~ waypoint + distance * c(0, -1),
    direction == "E" ~ waypoint + distance * c(1, 0),
    direction == "W" ~ waypoint + distance * c(-1, 0),
    direction == "L" ~ move_left(waypoint, distance),
    direction == "R" ~ move_right(waypoint, distance),
    direction == "F" ~ waypoint
  )
  return(current)
}

move_ship <- function(direction, distance, waypoint, ship) {
  if(direction == "F") {
    ship <- ship + distance * waypoint
  } 
  ship
}

ship <- c(0, 0)
waypoint <- c(10, 1)

for(i in 1:nrow(dd)) {
  new_waypoint <- move_waypoint(dd$direction[i], dd$distance[i], waypoint, ship)
  ship <- move_ship(dd$direction[i], dd$distance[i], waypoint, ship)
  waypoint <- new_waypoint
  print(c(ship, waypoint))
}
sum(abs(ship)) #second star!








