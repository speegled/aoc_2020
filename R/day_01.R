dd <- read.csv("data/day_01.csv", header = FALSE)
dd <- dd$V1

ind <- which(outer(dd, dd, function(x, y) x + y) == 2020)
outer(dd, dd, function(x, y) x * y)[ind[1]] #first star

ind <- which(outer(outer(dd, dd, function(x, y) x + y), dd, function(x, y) x + y) == 2020)
outer(outer(dd, dd, function(x, y) x * y), dd, function(x, y) x * y)[ind[1]] #second star
