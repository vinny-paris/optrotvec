#'Design and Omega Calculator
#'
#'Finds the Omega value for an augmented design created within the fucntion
#'
#'
#'@param rotv The rotation vector with elements 0,1,2 of length f for f being number of factors
#'@param design The design matrix to be expanded with elements 0,1,2
#'@param unique Should the design have only unique rows?
#'
#'@return Omega value for the agumented design 

tester <- function(rotv, design, unique = TRUE){
  d <- rot(design, rotv, unique)
  value <- omega(d)
  return(value)
}
