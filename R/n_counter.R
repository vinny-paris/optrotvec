#'Design Row Calculator
#'
#'Finds the number of rows for an augmented design created within the fucntion
#'
#'
#'@param rotv The rotation vector with elements 0,1,2 of length f for f being number of factors
#'@param design The design matrix to be expanded with elements 0,1,2
#'@param unique Should the design have only unique rows?
#'
#'@return number of rows

n_counter <- function(rotv_unq, design){
  d <- rot(design, rotv_unq[-1], unique = rotv_unq[1])
  value <- dim(d)[1]
  return(value)
}
