#' Generate a Random 3-Level Design
#' 
#' This will create a random 3 level design. The design may not be an orthogonal array or even full column rank.
#' 
#' @export
#' 
#' @param trt_num The maximum number of rows you want your design to have
#' @param var_num The number of 3-level factors you would like
#' 
#' @return A matrix randomy filled with 0's, 1', and 2's with var_num columns.
#' 
#' @examples 
#' \donttest{
#' ran_D(15, 3)
#' }
#' 
#' 
ran_D <- function(trt_num, var_num){
  rr <- c(0,1,2)
  X <- matrix(sample(rr, trt_num*var_num, replace = TRUE), ncol = var_num)
  X <- unique.matrix(X,ordered=FALSE)
  return(X)
}
