#' Determinant of Inverse of Info. Matrix
#' 
#' Given a deisgn encoded with 0,1,2 this will calculate the value of the inverse information matrix
#' 
#' @param design The encoded design matrix
#' 
#' @return Determant value
#' 
#' @example 
#' \dontrun{
#' j <- ran_D(20, 4)
#' d_opt(j)
#' }


d_opt <- function(design){
  d <- cbind(1, g_matrix_convert(design))
  val <- det(solve(t(d) %*% d))
  return(val)
}
  