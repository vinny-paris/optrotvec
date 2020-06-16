#' Alternative for design creation and det calculation
#' 
#' This is similar to it's twin tester_d2 but allows for the determinant to be calculated for 2-level and 3-level mixed designs.
#' 
#' @param rotv The rotation vector to be applied column wise.
#' @param design The design coded 0,1 for the rotation to be applied to
#' @param inv Should the det of the inverse be calculated
#' 
#' @return The determinant of the information matrix is returned
#' 


tester_partial2 <- function(rotv, design, inv = FALSE){
  d <- rot(design, rotv, unique = FALSE)
  if(inv) {value <- d_opt(d)}
  else {value <- d_opt_noinv(d, reducer = TRUE)}
  return(value)
}
