#' Foldover Design to introduce new levels
#' 
#' Using the foldover design it is possible to increase the number of levels in a design. This function focuses on the optimum strategry to turn a two level design into a three level design.
#' 
#' @param design The two level design coded 0,1 (intercept included) to be expanded
#' @param return_n The number of candidate rotation vectors to return. The default is 5.
#' 
#' 
#' 
#' @return This will return a data frame of return_n by f + 1 for f being the number of factors in the original design.
#' The fist column is 'Det' which is the determinant of the information matrix for the design created using that rotation vector
#' The remaining columns are the vectors corresponding to the elements of the rotation vector.
#' 
#' @example 
#' d2 <- ran_D2(25, 7)
#' opt_2level_exp(d2, return_n = 10)
#'   


opt_2level_exp <- function(design, return_n = 5){
  inv <- FALSE
  
  #warnings 
  if(return_n %% 1 != 0){stop("Please make the return_n parameter a natural number!", immediate. = TRUE)}
  if(sum(class(design) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", immediate. = TRUE)}

  
  d <- as.matrix(design)
  
  f <- dim(d)[2]
 
  all_rot_vec <- optrotvec:::alias_design(f)
  s <- sample(dim(all_rot_vec)[1])
  all_rot_vec <- all_rot_vec[s,]
  jj <<- all_rot_vec
  all_rot_vec <- all_rot_vec[apply(all_rot_vec, 1, function(r) !any(r %in% 0)),]
  
  
               
  alts     <- rbind(all_rot_vec)
  
  if(length(dim(candidate_vecs)) == 0){candidate_vecs <- as.matrix(t(candidate_vecs))}
  hope <- cbind(candidate_vecs)

  
  dets <- apply(hope, 1, tester_d2, design = d, inv = inv)
 
  

  finals <- cbind(dets^(1/(2*f + 1)), matrix(hope, ncol = f))
  
  finals <- data.frame(finals)
  colnames(finals)[c(1,2)] <- c('Det',  'Rotation Vectors')
  finals <- arrange(as.data.frame(finals), desc(dets))[c(1:return_n),]
  return(finals)
}



