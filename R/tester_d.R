#'Design and Determinant Calculator
#'
#'Finds the determinant for an augmented design's information matrix created within the fucntion
#'
#'
#'@param rotv The rotation vector with elements 0,1,2 of length f for f being number of factors
#'@param design The design matrix to be expanded with elements 0,1,2
#'
#'@return Determinant for the agumented design's information matrix

tester_d <- function(rotv, design){
  d <- rot(design, rotv, unique = FALSE)
  value <- d_opt_noinv(d)
  return(value)
}
