#' Tranforms a 0-1-2 design matrix to a 0-1 matrix
#' 
#' @param  X The encoded design matrix in 0-1-2's
#' 
#' @return Each factor gains two columns, with values (0,0) if the factor is 0, (1,0) if the factor is 1, and (0,1) is the factor is 2.
#' 





g_matrix_convert <- function(X){
  holding <- NULL
  kathy <- apply(X, 1:2, encoder)
  j <- matrix(as.matrix(kathy), ncol = 3, byrow = TRUE)
  jj <- kronecker(c(1:(dim(X)[2])), rep(1, dim(X)[1]))
  k <- cbind(jj, j)
  
  for(i in unique(k[,1])){
    ww <- subset(k, k[,1] == i)
    ww <- ww[, -c(1,2)]
    holding <- cbind(holding, ww)
  }
  
  return(holding)
}