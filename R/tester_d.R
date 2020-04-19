#'Design and Determinant Calculator
#'
#'Finds the determinant for an augmented design's information matrix created within the fucntion
#'
#'
#'@param rotv The rotation vector with elements 0,1,2 of length f for f being number of factors
#'@param design The design matrix to be expanded with elements 0,1,2
#'@param inv Should we look at the detemenent of X1'X1 or the determenent of it's inverse?
#'
#'@return Determinant for the agumented design's information matrix

tester_d <- function(rotv, design, inv = FALSE){
  d <- rot(design, rotv, unique = FALSE)
  if(inv) {value <- d_opt(d)}
  else {value <- d_opt_noinv(d)}
  return(value)
}
