#' Internal encoding function
#' 
#' Move along please
#' 
#' 
#' @param  x a 0, 1, or 2 value
#' 
#' @return A 3 element vector that is (1,0,0) for x = 0, (0,1,0) for x = 1, and (0,0,1) for x = 2. 
#' 


encoder <- function(x){
  if(x == 0) {x <- matrix(c(1,0,0), ncol = 1); return(x)}
  if(x == 1) {x <- matrix(c(0,1,0), ncol = 1); return(x)}
  if(x == 2) {x <- matrix(c(0,0,1), ncol = 1); return(x)}
}