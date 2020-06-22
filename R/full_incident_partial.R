#' Creates List of Frequencies of Occurances for Paritial Expansions
#' 
#' @description This is a purely internatalo function that creates tables of incidents for each factor at each level
#' 
#' @param e A matrix where each pair of columns are represented next to each other
#' @param tt1 A string name for all possible comibnations. Needed incase the frequency is 0.
#' @param tt2 Character strings for 2/2 level factors
#' @param tt3 Character strings for 3/2 level factors





full_incident_partial <- function(e, tt1, tt2, tt3){
  f <- dim(e)[2]/2
  k <- NULL
  holding <- list(NA)
  for(x in 1:f){
    holding <- cat_and_count_partial(e[,c(x, x+1)], tt1, tt2 tt3)
    k <- rbind(holding, k)
  }
  return(k)
}