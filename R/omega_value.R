#' Finds the Omega value for a given design
#' 
#' The function takes in a single 3-level design and returns the calculated value for Omega.
#' 
#' @export
#' 
#' @param D The design matirx encoded with (0,1,2) where each run is a row. 
#' 
#' @importFrom tidyr expand
#' 
#' @return 
#' \item{Omega-value}{The value the Omega optimality criterion for this design.}
#' 



omega <- function(D){
  #get the bias table and drop the intercept row
  bias <- alias_matrix(D)
  t <- bias[-1, ]
  f <- dim(D)[2]
  
  omega <- 0

  row_need <- 2*(1:f) - 1
  col_need <- 2*(1:(f*(f-1))) - 1
  dd <- data.frame(row_need, col_need)
  combs <- expand(dd, dd[,1], dd[,2])
  
  final <- sum(apply(combs, 1, submatrix_counter, alias = t))
  return(final)
  names(final) <- "Omega value"
}
  
