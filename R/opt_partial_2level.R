#' Foldover Design to introduce new levels to a subset of factors
#' 
#' Using the foldover design it is possible to increase the number of levels in a design. This function focuses on the optimum strategry to turn a two level design into a three level design. Or, as in this function, a subset of those 2-level factors to 3-level factors.
#' 
#' @param design The two level design coded 0,1 (intercept included) to be expanded
#' @param return_n The number of candidate rotation vectors to return. The default is 5.
#' @param cl A vector of the location of columns that should be exapnded from 2 levels to 3 levels.  
#' @param opt What optimality criterion should be used? Choices are 'Det' for the largest determinants or 'Min_Incident' which returns the max of the min of the incidnets of two factor levels.
#' 
#' 
#' @export
#' 
#' @return This will return a data frame of return_n by f + 1 for f being the number of factors in the original design.
#' The fist column is 'Det' which is the determinant of the information matrix for the design created using that rotation vector
#' The remaining columns are the vectors corresponding to the elements of the rotation vector.
#' 
#' 
#' @importFrom dplyr arrange
#' @importFrom dplyr desc



opt_partial_2level <- function(design, return_n = 5, cl, opt = 'Det'){
   inv <- FALSE
  
  #warnings 
  if(return_n %% 1 != 0){stop("Please make the return_n parameter a natural number!", immediate. = TRUE)}
  if(sum(class(design) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", immediate. = TRUE)}

  
  d <- as.matrix(design)
  
  f <- dim(d)[2]
 
  #create and randomize rotation vectors
  all_rot_vec <- alias_design(length(cl))
  s <- sample(dim(all_rot_vec)[1], replace = FALSE)
  all_rot_vec <- all_rot_vec[s,]

  #creates rotation vectors with 0's in the non interesting colums
  mat <- matrix(c(rep(0, (f*dim(all_rot_vec)[1]))), ncol = f)
  mat[,cl] <- all_rot_vec[,1:length(cl)]

  
  #calculate the determinants
  dets <- apply(mat, 1, tester_partial2, design = d, inv = inv)
  
  #calculate the min incidents
  inc_min_list <- apply(mat, 1, tester_inc2_partial, design = d)
  inc_min <- do.call(rbind.data.frame, inc_min_list)
  inc_min <- apply(inc_min, 2, as.numeric)
  

  finals <- cbind(dets^(1/(2*f + 1)), inc_min, matrix(mat, ncol = f))
  finals <- data.frame(finals)
   colnames(finals)[1:4] <- c('Det',  'Min. Incident', 'Frequency', 'Rotation Vectors')
  
     #rankings of the rotation vectors for a given optimality criterion.
  if(opt == 'Det') {finals <- arrange(as.data.frame(finals), desc(dets))[c(1:return_n),]}
  if(opt == 'Min_Incident') {finals <- arrange(as.data.frame(finals), desc(inc_min[,1]), (inc_min[,2]), desc(dets))[c(1:return_n),]}

   
   
    return(finals)
}



 
  