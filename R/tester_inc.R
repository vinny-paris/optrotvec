#' Wrapper to apply an incident counter
#' 
#' In general this function works similarly to tester_D and tester_D2
#' 
#' @param rotv The rotation vector to be used
#' @param design The original coded 0/1/2 design
#' 
#' 


tester_inc <- function(rotv, design){
 d <- rot(design, rotv, unique = FALSE)
 e <- tab_inc(d)
 e <- as.data.frame(e)
 g <- e[1,]
 return(g)
}




  
  
  







