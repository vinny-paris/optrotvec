#' Randomly Build a 2-level Design
#' 
#' This fucntion randomly builds a 2-level design that allws for unique or non unique runs.
#' 
#' @param n_rows The number of rows you would like in the final design. If unique = TRUE then this is the upper bound.
#' @param factors The number of factors you would like in the experiment
#' @param unique Should replicate runs be allowed?
#' 
#' @export 
#' 
#' @return A design data frame with the number of columns being the nubmer of factors. 



ran_D2 <- function(n_rows, factors, unique = FALSE){
  k <- sample(c(0,1), n_rows*factors, replace = TRUE)
  des <- matrix(k, ncol = factors)
  if(unique == TRUE){des <- unique(des)}
  return(des)
}
  