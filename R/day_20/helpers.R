find_edge_orientation <- function(mat, edge, position) {
  #browser()
  if(position == "right") {
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[,10], collapse = "") == edge) {
        return(mat)
      }
    }
    mat <- t(mat)
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[,10], collapse = "") == edge) {
        return(mat)
      }
    }
  }
  if(position == "left") {
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[,1], collapse = "") == edge) {
        return(mat)
      }
    }
    mat <- t(mat)
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[,1], collapse = "") == edge) {
        return(mat)
      }
    }
  }
  if(position == "top") {
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[1,], collapse = "") == edge) {
        return(mat)
      }
    }
    mat <- t(mat)
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[1,], collapse = "") == edge) {
        return(mat)
      }
    }
  }
  if(position == "bottom") {
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[10,], collapse = "") == edge) {
        return(mat)
      }
    }
    mat <- t(mat)
    for(i in 1:4) {
      mat <- rotate(mat)
      if(paste(mat[10,], collapse = "") == edge) {
        return(mat)
      }
    }
  }
  return(NA)
}


put_matrix <- function(mat, bb, row, col) {
  #browser()
  rows <- ((row - 1) * 10 + 1):(row * 10)
  cols <- ((col - 1) * 10 + 1):(col * 10)
  bb[rows, cols] <- mat
  bb
}