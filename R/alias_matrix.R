#' Creates the Alias Matrix
#' 
#' 
#' @export
#' 
#' @param X an encoded first order design matrix of 0's, 1's, and 2's with f columns. 
#' 
#' @return The alias matrix that corresponds to all second order interactions compared to the main effects of X.
#' 
#' @examples 
#' \dontrun{
#' X <- ran_D(15, 3)
#' alias_matrix(X)
#' 
#' X1 <- rot(X, c(1,1,1))
#' alias_matrix(X1)
#' }



alias_matrix <- function(X){
  X2 <- oa_check(X)[,-c(1:(dim(X)[2]))]
  nam <- colnames(X2)
  Xi <- cbind(1, g_matrix_convert(X))
  X2i <- g_matrix_convert(X2)
  bias <- solve(t(Xi) %*% Xi) %*% t(Xi) %*% X2i
  return(bias)
}
