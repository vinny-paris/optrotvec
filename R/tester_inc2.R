#' Wrapper to apply an incident counter for 0/1 designs
#' 
#' In general this function works similarly to tester_D and tester_D2 and is the twin of tester_inc with the exception this is for 0/1 designs only.
#' 
#' @param rotv The rotation vector to be used
#' @param design The original coded 0/1/2 design
#' 
#' 

tester_inc2 <- function(rotv, design){
 d <- rot(design, rotv, unique = FALSE)
 e <- tab_inc2(d)
 e <- as.data.frame(e)
 g <- e[1,]
 return(g)
}