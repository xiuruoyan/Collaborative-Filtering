combine <- function(weight_mat, threshold, n){
  diag(weight_mat) <- 0
  thresh_neighbor <- list()
  select_neighbor <- list()
  nuser <- nrow(weight_mat)
  
  for (i in 1:nuser){
    thresh_neighbor[[i]] <- which(abs(weight_mat[i,]) > threshold)
    if (length(thresh_neighbor[[i]]) < n) {
      select_neighbor[[i]] <- thresh_neighbor[[i]]
    } else {
      select_neighbor[[i]] <- order(weight_mat[i, ], decreasing = T)[1:n]
    }
  }
  return(select_neighbor)
}