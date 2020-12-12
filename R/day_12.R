library(tidyverse)

inc <- function(x, n, m = 4) { #helper function
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
#' Start of second star, with rotation matrix
#'

rotation_matrix <- matrix(c(0, 1, -1, 0), nrow = 2)

rotate_waypoint <- function(waypoint, direction, signed_magnitude) {
  pow <- signed_magnitude / 90
  as.vector(matrixcalc::matrix.power(rotation_matrix, pow) %*% waypoint)
}

dd <- dd %>% 
  mutate(signed_distance = ifelse(direction == "R", -1 * distance, distance)) %>% 
  mutate(signed_direction = ifelse(direction == "R", "L", direction))

move_waypoint <- function(direction, distance, waypoint, ship) {
  wp <- switch(direction,
               N = waypoint + distance * c(0, 1),
               S = waypoint + distance * c(0, -1),
               E = waypoint + distance * c(1, 0),
               W = waypoint + distance * c(-1, 0),
               L = rotate_waypoint(waypoint, direction, distance),
               F = waypoint
  )
  return(wp)
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
  new_waypoint <- move_waypoint(dd$signed_direction[i], dd$signed_distance[i], waypoint, ship)
  ship <- move_ship(dd$direction[i], dd$distance[i], waypoint, ship)
  waypoint <- new_waypoint
}
sum(abs(ship)) #second star!




#'
#' Start of second star, original
#'

move_left <- function(wp, di) {
  #assume di a multiple of 90
  di <- (di / 90) 
  for(j in 1:di) {
    wp <- rev(wp)
    wp[1] <- -wp[1]
  }
  return(wp)
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
    direction == "L" ~ move_left(waypoint, distance), #this is bad because it executes even when direction == "other" so if it throws an error in any case, we get an error
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








