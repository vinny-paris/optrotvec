#' Rearrange (and copy) the columns of a matrix
#' 
#' @param d The design matrix
#' @param cols object that can be coerced into a numeric vector such that cols should 


col_copier <- function(d, cols){
  e <- d[,as.vector(cols)]
  return(e)
}