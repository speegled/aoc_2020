dd <- read.csv("data/day_01.csv", header = FALSE)
dd <- dd$V1

ind <- which(outer(dd, dd, function(x, y) x + y) == 2020)
outer(dd, dd, function(x, y) x * y)[ind[1]] #first star

ind <- which(outer(outer(dd, dd, function(x, y) x + y), dd, function(x, y) x + y) == 2020)
outer(outer(dd, dd, function(x, y) x * y), dd, function(x, y) x * y)[ind[1]] #second star


#'
#' Not sure why I decided to do this using outer. Maybe because I didn't have the 
#' data in hand when I was working on it due to a glitch on day one. I think a more
#' natural, but less R like, approach would have been:
#'

for(a in dd) {
  for(b in dd) {
    if(a + b == 2020) {
      print(a * b)
    }
  }
}

for(a in dd) {
  for(b in dd) {
    for(c in dd) {
      if(a + b + c == 2020) {
        print(a * b * c)
      }
    }
  }
}
