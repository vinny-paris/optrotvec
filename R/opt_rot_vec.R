#' Finds the optimal foldover plans for 3 level designs
#' 
#' For a given design matrix encoded with 0,1,2 this function will return the values of Omega, the determinant of the inverse of X_1'X_1, and 
#' 
#' @export
#' 
#' @param design The encoded design matrix using 0,1,2 notation that is to be expanded
#' @param return_n How many of the top rotation vectors (with regards to Omega) should be returned? Default is 5.
#' @return Returns a data frame that is return_n by f + 2.  
#' Omega-value. This will be the first column
#' Determenant of the information matrix
#' Det. Ratio. This is the ratio of the deteminant column over the the detemenant of the information matrix for a design that is just a tripling of the original.
#' Rotation vectors
#' 
#' @examples
#' \dontrun{
#' d <- ran_D(25, 4)
#' opt_rot_vec(d, return_n) 
#' }
#' 


opt_rot_vec <- function(design, return_n = 5){
  inv <- FALSE
  
  #warnings 
  if(return_n %% 1 != 0){stop("Please make the return_n parameter a natural number!", immediate. = TRUE)}
  if(sum(class(design) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", immediate. = TRUE)}
  if(sum(sort(unique(as.vector(design))) == c(0, 1, 2)) != 3) {stop("Please code the matrix with 0, 1 and 2's only!", immediate. = TRUE)}
  
  
  d <- design
  
  f <- dim(d)[2]
 
  all_rot_vec <- alias_design(f)
  s <- sample(dim(all_rot_vec)[1])
  all_rot_vec <- all_rot_vec[s,]
  
  omegas <- apply(all_rot_vec, 1, tester, design = d)
  candidate_vecs <- all_rot_vec[order(omegas, decreasing = TRUE)[1:return_n],]
  if(length(dim(candidate_vecs)) == 0){candidate_vecs <- as.matrix(t(candidate_vecs))}
  
  dets <- apply(candidate_vecs, 1, tester_d, design = d, inv = inv)
  det_org <- tester_d(design = d, rotv = rep(0,f), inv = inv)
                  
  best_omegas <- omegas[order(omegas, decreasing = TRUE)[1:return_n]]
  
  
  finals <- cbind(best_omegas, dets, dets/det_org, matrix(candidate_vecs, ncol = f))
  if(class(finals) == 'vector'){finals <- as.data.frame(t(finals))}
  colnames(finals)[1:4] <- c('Omega value', 'Determinant', 'Det Ratio', 'Rotation Vectors')
  finals <- finals[order(-finals[,1], -finals[,2]), ]
  return(finals)
}
