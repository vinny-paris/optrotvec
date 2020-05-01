#' Check's Alias submatrix for 0's
#' 
#'  This funciton is basically a logic function to see if corrdinates v of design is a submatrix of the alias function that has elements of all 0's
#'  
#'   
#'
#'
#'@param v The top left of a 2x2 submatrix coordinates
#'@param alias An alias matrix of interest
#'
#'@return 1 if all four elements are 0, 0 otherwise
#'
#'@examples
#'\dontrun{
#'d <- ran_D(25, 4)
#'alias <- alias_matrix(d)
#'submatrix_counter(c(3,5), alias)
#'alias_2 <- alias[c(3,4), c(5,6)]
#'submatrix_counter(c(3,5), alias_2)
#'}

  submatrix_counter <- function(v, alias){
     i <- v[1]
     j <- v[2]
     x <- alias[i:(i+1), j:(j+1)]
     if(sum(abs(x) < 1e-8) == 4) {
       return(1)}
      else{return(0)}
   }
   