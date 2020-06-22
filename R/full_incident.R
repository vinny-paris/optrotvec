#' Creates List of Frequencies of Occurances
#' 
#' @description This is a purely internatalo function that creates tables of incidents for each factor at each level
#' 
#' @param e A matrix where each pair of columns are represented next to each other
#' @param tt A string name for all possible comibnations. Needed incase the frequency is 0.
#' 



full_incident <- function(e, tt){
  f <- dim(e)[2]/2
  holding <- list(NA)
  for(x in 1:f){
    holding <- cat_and_count(e[,c(x, x+1)], tt)
    k <- rbind(holding, k)
  }
  return(k)
}