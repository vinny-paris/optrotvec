#' Determinant of Info. Matrix
#' 
#' Given a deisgn encoded with 0,1,2 this will calculate the value of the information matrix
#' 
#' @param design The encoded design matrix
#' @param reducer This is used to correct for an encoded matrice with both 2-level and 3-level factors.
#' 
#' @return Determant value
#' 
#' @examples 
#' \dontrun{
#' j <- ran_D(20, 4)
#' d_opt_noinv(j)
#' }


d_opt_noinv <- function(design, reducer = FALSE){
  d <- cbind(1, g_matrix_convert(design))
  if(reducer == TRUE){d <- d[,apply(d, 2, function(x) length(unique(x)) != 1)]}
  val <- det(t(d) %*% d)
  return(val)
}
  
  