#'Design Row Calculator
#'
#'Finds the number of rows for an augmented design created within the fucntion
#'
#'
#'@param rotv_unq A vector with the first entry being TRUE/FALSE for if the design should be unique. The rest of the vector is the rotation vector with elements 0,1,2 of length f for f being number of factors
#'@param design The design matrix to be expanded with elements 0,1,2
#'
#'@return number of rows

n_counter <- function(rotv_unq, design){
  d <- rot(design, rotv_unq[-1], unique = rotv_unq[1])
  value <- dim(d)[1]
  return(value)
}
