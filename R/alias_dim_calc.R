#' Number of Generators + 1
#'
#' Gives total number of generators for a set number of defining generators plus 1
#' 
#' @param x The number of defining generators
#' 
#' @return A single number. This will be the total number of generators in the alias structure built by the defining generators plus 1.
#' 
#'
#' 
#' @note The reason for the plus one is for simplification in other coding parts of the package

alias_dim_calc <- function(x){
  
 t <- (3^x + 1)/2
  
  return(t) 
}


