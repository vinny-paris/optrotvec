#' Determinant of Info. Matrix
#' 
#' Given a deisgn encoded with 0,1,2 this will calculate the value of the information matrix
#' 
#' @param design The encoded design matrix
#' 
#' @return Determant value
#' 
#' @examples 
#' \dontrun{
#' j <- ran_D(20, 4)
#' d_opt_noinv(j)
#' }


d_opt_noinv <- function(design){
  d <- cbind(1, g_matrix_convert(design))
  val <- det(t(d) %*% d)
  return(val)
}
  
  